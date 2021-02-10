{-
 - Copyright (C) 2019  Koz Ross <koz.ross@retro-freedom.nz>
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module:        Data.Finitary.PackBytes
-- Description:   Scheme for byte-packing @Finitary@ types.
-- Copyright:     (C) Koz Ross 2019
-- License:       GPL version 3.0 or later
-- Stability:     Experimental
-- Portability:   GHC only
--
-- If a type @a@ is 'Finitary', each inhabitant of @a@ has an index, which can
-- be represented as a byte string of a fixed length (as the number of indexes
-- is finite). Essentially, we can represent any value of @a@ as a fixed-length
-- string over an alphabet of cardinality \(256\). Based on this, we can derive
-- a 'VU.Unbox' instance, representing a 'VU.Vector' as a large byte string.
-- This also allows us to provide a 'Storable' instance for @a@.
--
-- This encoding is fairly tight in terms of space use, especially for types
-- whose cardinalities are large. Additionally, byte-access is considerably
-- faster than bit-access on most architectures. If your types have large
-- cardinalities, and minimal space use isn't a concern, this encoding is good.
--
-- Some architectures prefer whole-word access - on these, there can be some
-- overheads using this encoding. Additionally, the encoding and decoding step
-- for this encoding is longer than the one for "Data.Finitary.PackWords". If 
-- @Cardinality a < Cardinality Word@, you should 
-- consider a different encoding - in particular, check "Data.Finitary.PackInto", 
-- which is more flexible and faster, with greater control over space usage.
module Data.Finitary.PackBytes 
  (
  -- * Packing and unpacking between a type and a 'ByteArray'
    PackBytes(.., Packed)

  -- * Helpers
  , intoBytes, outOfBytes
  )
  where

-- base
import Data.Kind (Type)
import Data.Word (Word8)
import Data.Hashable (Hashable(..), hashByteArrayWithSalt)
import Foreign.Storable (Storable(..))
import GHC.Exts
import GHC.IO
import GHC.TypeNats

-- binary
import qualified Data.Binary as Bin

-- coercible-utils
import CoercibleUtils (op, over, over2)

-- deepseq
import Control.DeepSeq (NFData(..))

-- finitary
import Data.Finitary (Finitary(..))

-- finite-typelits
import Data.Finite.Internal (Finite(..), getFinite)

-- ghc-typelits-extra
import GHC.TypeLits.Extra

-- primitive
import Control.Monad.Primitive (PrimMonad(primitive))
import Data.Primitive.ByteArray (ByteArray(..), MutableByteArray(..))

-- vector
import qualified Data.Vector.Unboxed.Base      as VU
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Primitive         as VP
import qualified Data.Vector.Generic.Mutable   as VGM
import qualified Data.Vector.Primitive.Mutable as VPM

-- vector-binary-instances
import Data.Vector.Binary ()

-- vector-instances
import Data.Vector.Instances ()

#ifdef BIGNUM
-- base
import Numeric.Natural (Natural)

-- ghc-bignum
import GHC.Num.Integer (integerToNaturalClamp)
import GHC.Num.Natural (naturalFromByteArray#, naturalToMutableByteArray#)
#else
-- integer-gmp
import GHC.Integer.GMP.Internals
  ( importIntegerFromByteArray, exportIntegerToMutableByteArray )
#endif

--------------------------------------------------------------------------------

-- | An opaque wrapper around @a@, representing each value as a byte array.
newtype PackBytes (a :: Type) = PackedBytes ByteArray
  deriving (Eq, Show)

type role PackBytes nominal

-- | To provide (something that resembles a) data constructor for 'PackBytes', we
-- provide the following pattern. It can be used like any other data
-- constructor:
--
-- > import Data.Finitary.PackBytes
-- >
-- > anInt :: PackBytes Int
-- > anInt = Packed 10
-- >
-- > isPackedEven :: PackBytes Int -> Bool
-- > isPackedEven (Packed x) = even x
--
-- __Every__ pattern match, and data constructor call, performs a
-- \(\Theta(\log_{256}(\texttt{Cardinality a}))\) encoding or decoding of @a@.
-- Use with this in mind.
{-# COMPLETE Packed #-}
pattern Packed :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  a -> PackBytes a
pattern Packed x <- (unpackBytes -> x)
  where Packed x = packBytes x

instance (Finitary a, 1 <= Cardinality a) => Ord (PackBytes a) where
  compare (PackedBytes (ByteArray arr1)) (PackedBytes (ByteArray arr2)) =
    compareByteArraysLE arr1 arr2 ( nbBytes -# 1# )
      where
        nbBytes :: Int#
        !(I# nbBytes) = byteLength @a

-- Re-use primitive vector instance for 'Binary'.
instance (Finitary a, 1 <= Cardinality a) => Bin.Binary (PackBytes a) where
  {-# INLINE put #-}
  put = Bin.put . VP.Vector @Word8 0 (byteLength @a) . op PackedBytes
  {-# INLINE get #-}
  get = PackedBytes . ( \ ( VP.Vector _ _ ba :: VP.Vector Word8 ) -> ba ) <$> Bin.get
            --                        ^
            -- binary instance for ( VP.Vector Word8 ) always returns 0 offset

instance (Finitary a, 1 <= Cardinality a) => Hashable (PackBytes a) where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt = ( \ ( ByteArray ba ) -> hashByteArrayWithSalt ba 0 (byteLength @a) salt )
                    . op PackedBytes

instance NFData (PackBytes a) where
  {-# INLINE rnf #-}
  rnf = rnf . op PackedBytes

instance (Finitary a, 1 <= Cardinality a) => Finitary (PackBytes a) where
  type Cardinality (PackBytes a) = Cardinality a
  {-# INLINE fromFinite #-}
  fromFinite = PackedBytes . intoBytes
  {-# INLINE toFinite #-}
  toFinite = outOfBytes . op PackedBytes

instance (Finitary a, 1 <= Cardinality a) => Bounded (PackBytes a) where
  {-# INLINE minBound #-}
  minBound = start
  {-# INLINE maxBound #-}
  maxBound = end

instance (Finitary a, 1 <= Cardinality a) => Storable (PackBytes a) where
  {-# INLINABLE sizeOf #-}
  sizeOf _ = byteLength @a
  {-# INLINABLE alignment #-}
  alignment _ = alignment (undefined :: Word8)
  {-# INLINABLE peek #-}
  peek (Ptr addr) =
    IO $ \ s1 ->
      case newByteArray# nbBytes s1 of
        (# s2, mba #) -> case copyAddrToByteArray# addr mba 0# nbBytes s2 of
          s3 -> case unsafeFreezeByteArray# mba s3 of
            (# s4, ba #) -> (# s4, PackedBytes (ByteArray ba) #)
    where
      nbBytes :: Int#
      !(I# nbBytes) = byteLength @a
  {-# INLINE poke #-}
  poke (Ptr addr) (PackedBytes (ByteArray ba)) =
    IO $ \ s1 ->
      case copyByteArrayToAddr# ba 0# addr nbBytes s1 of
        s2 -> (# s2, () #)
    where
      nbBytes :: Int#
      !(I# nbBytes) = byteLength @a

newtype instance VU.MVector s (PackBytes a) = MV_PackBytes (VU.MVector s Word8)

instance (Finitary a, 1 <= Cardinality a) => VGM.MVector VU.MVector (PackBytes a) where
  {-# INLINE basicLength #-}
  basicLength = over MV_PackBytes ((`div` byteLength @a) . VGM.basicLength)
  {-# INLINE basicOverlaps #-}
  basicOverlaps = over2 MV_PackBytes VGM.basicOverlaps
  {-# INLINABLE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over MV_PackBytes (VGM.basicUnsafeSlice (i * byteLength @a) (len * byteLength @a))
  {-# INLINABLE basicUnsafeNew #-}
  basicUnsafeNew len = MV_PackBytes <$> VGM.basicUnsafeNew (len * byteLength @a)
  {-# INLINE basicInitialize #-}
  basicInitialize = VGM.basicInitialize . op MV_PackBytes
  {-# INLINABLE basicUnsafeRead #-}
  basicUnsafeRead (MV_PackBytes (VU.MV_Word8 (VPM.MVector (I# off) _ (MutableByteArray full_mba)))) (I# i) =
    primitive $ \ s1 ->
      case newByteArray# nbBytes s1 of
        (# s2, elem_mba #) -> case copyMutableByteArray# full_mba (off +# nbBytes *# i) elem_mba 0# nbBytes s2 of
          s3 -> case unsafeFreezeByteArray# elem_mba s3 of
            (# s4, elem_ba #) -> (# s4, PackedBytes (ByteArray elem_ba) #)
    where
      nbBytes :: Int#
      !(I# nbBytes) = byteLength @a
  {-# INLINABLE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_PackBytes (VU.MV_Word8 (VPM.MVector (I# off) _ (MutableByteArray full_mba)))) (I# i) (PackedBytes (ByteArray val_ba)) =
    primitive $ \ s1 -> case copyByteArray# val_ba 0# full_mba (off +# nbBytes *# i) nbBytes s1 of
      s2 -> (# s2, () #)
      where
        nbBytes :: Int#
        !(I# nbBytes) = byteLength @a

newtype instance VU.Vector (PackBytes a) = V_PackedBytes (VU.Vector Word8)

instance (Finitary a, 1 <= Cardinality a) => VG.Vector VU.Vector (PackBytes a) where
  {-# INLINE basicLength #-}
  basicLength = over V_PackedBytes ((`div` byteLength @a) . VG.basicLength)
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = fmap V_PackedBytes . VG.basicUnsafeFreeze . op MV_PackBytes
  {-# INLINE basicUnsafeThaw #-} 
  basicUnsafeThaw = fmap MV_PackBytes . VG.basicUnsafeThaw . op V_PackedBytes
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over V_PackedBytes (VG.basicUnsafeSlice (i * byteLength @a) (len * byteLength @a))
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_PackedBytes (VU.V_Word8 (VP.Vector (I# off) _ (ByteArray full_ba)))) (I# i) =
    pure $ runRW# $ \ s1 ->
      case newByteArray# nbBytes s1 of
        (# s2, elem_mba #) -> case copyByteArray# full_ba (off +# nbBytes *# i) elem_mba 0# nbBytes s2 of
          s3 -> case unsafeFreezeByteArray# elem_mba s3 of
            (# _, elem_ba #) -> PackedBytes (ByteArray elem_ba)
    where
      nbBytes :: Int#
      !(I# nbBytes) = byteLength @a

instance (Finitary a, 1 <= Cardinality a) => VU.Unbox (PackBytes a)

-- Helpers

type ByteLength a = NatBytes (Cardinality a)
type NatBytes n = CLog (Cardinality Word8) n

{-# INLINE byteLength #-}
byteLength :: forall (a :: Type) (b :: Type) . 
  (Finitary a, 1 <= Cardinality a, Num b) =>
  b
byteLength = fromIntegral $ natVal' @(ByteLength a) proxy#

{-# INLINE natBytes #-}
natBytes :: forall (n :: Nat) (b :: Type) .
  (KnownNat n, 1 <= n, Num b) =>
  b
natBytes = fromIntegral $ natVal' @(NatBytes n) proxy#

{-# INLINE packBytes #-}
packBytes :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  a -> PackBytes a
packBytes = fromFinite . toFinite

{-# INLINE unpackBytes #-}
unpackBytes :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  PackBytes a -> a
unpackBytes = fromFinite . toFinite

{-# INLINABLE compareByteArraysLE #-}
compareByteArraysLE :: ByteArray# -> ByteArray# -> Int# -> Ordering
compareByteArraysLE ba1 ba2 off
  | isTrue# ( off <# 0# )
  = EQ
  | isTrue# ( b1 `eqWord#` b2 )
  = compareByteArraysLE ba1 ba2 ( off -# 1# )
  | isTrue# ( b1 `ltWord#` b2 )
  = LT
  | otherwise
  = GT
  where
    b1, b2 :: Word#
    b1 = indexWord8Array# ba1 off
    b2 = indexWord8Array# ba2 off

-- | Pack a natural number less than @n@ into a 'ByteArray',
-- with the limbs stored in little-endian order.
{-# INLINABLE intoBytes #-}
intoBytes :: forall (n :: Nat) . 
  (KnownNat n, 1 <= n) => 
  Finite n -> ByteArray

-- | Unpack a natural number less than @n@ from a 'ByteArray',
-- in which the limbs are stored in little-endian order.
{-# INLINABLE outOfBytes #-}
outOfBytes :: forall (n :: Nat) . 
  (KnownNat n, 1 <= n) =>
  ByteArray -> Finite n

#ifdef BIGNUM

intoBytes f = runRW# $ \ s1 ->
  case newByteArray# nbBytes s1 of
    (# s2, mba #) -> case naturalToMutableByteArray# i mba 0## 0# s2 of
      (# s3, bytesWritten' #) ->
        let bytesWritten = word2Int# bytesWritten' in
          case setByteArray# mba bytesWritten (nbBytes -# bytesWritten) 0# s3 of
            s4 -> case unsafeFreezeByteArray# mba s4 of
              (# _, ba #) -> ByteArray ba
  where
    i :: Natural
    i = integerToNaturalClamp ( getFinite f )
    nbBytes :: Int#
    !(I# nbBytes) = natBytes @n

outOfBytes (ByteArray ba) = runRW# $ \ s1 ->
  case naturalFromByteArray# nbBytes ba 0## 0# s1 of
    (# _, nat #) -> Finite (toInteger nat)
  where
    nbBytes :: Word#
    !(W# nbBytes) = natBytes @n
  
#else

intoBytes f = runRW# $ \ s1 ->
  case newByteArray# nbBytes s1 of
    (# s2, mba #) ->
      let IO toMBA = exportIntegerToMutableByteArray i mba 0## 0# in
        case toMBA s2 of
          (# s3, W# bytesWritten' #) ->
            let bytesWritten = word2Int# bytesWritten' in
              case setByteArray# mba bytesWritten (nbBytes -# bytesWritten) 0# s3 of
                s4 -> case unsafeFreezeByteArray# mba s4 of
                  (# _, ba #) -> ByteArray ba
  where
    i :: Integer
    i = getFinite f
    nbBytes :: Int#
    !(I# nbBytes) = natBytes @n

outOfBytes (ByteArray ba) = Finite $ importIntegerFromByteArray ba 0## nbBytes 0#
  where
    nbBytes :: Word#
    !(W# nbBytes) = natBytes @n

#endif
