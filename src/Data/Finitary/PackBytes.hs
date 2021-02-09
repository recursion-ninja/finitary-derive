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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

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
  PackBytes, pattern Packed
) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits.Extra
import GHC.TypeNats
import CoercibleUtils (op, over, over2)
import Data.Kind (Type)
import Data.Word (Word8)
import Data.Vector.Binary ()
import Data.Vector.Instances ()
import Data.Hashable (Hashable(..))
import Control.DeepSeq (NFData(..))
import Data.Finitary (Finitary(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr, plusPtr)
import Numeric.Natural (Natural)
import Data.Finite (Finite)
import Control.Monad.Trans.State.Strict (evalState, get, modify, put)
import Data.Semigroup (Dual(..))

import qualified Data.Binary as Bin
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

-- | An opaque wrapper around @a@, representing each value as a byte string.
newtype PackBytes (a :: Type) = PackBytes (VU.Vector Word8)
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
pattern Packed :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  a -> PackBytes a
pattern Packed x <- (unpackBytes -> x)
  where Packed x = packBytes x

instance Ord (PackBytes a) where
  compare (PackBytes v1) (PackBytes v2) = getDual . VU.foldr go (Dual EQ) . VU.zipWith (,) v1 $ v2
    where go input order = (order <>) . Dual . uncurry compare $ input

instance Bin.Binary (PackBytes a) where
  {-# INLINE put #-}
  put = Bin.put . op PackBytes
  {-# INLINE get #-}
  get = PackBytes <$> Bin.get

instance Hashable (PackBytes a) where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt = hashWithSalt salt . op PackBytes

instance NFData (PackBytes a) where
  {-# INLINE rnf #-}
  rnf = rnf . op PackBytes

instance (Finitary a, 1 <= Cardinality a) => Finitary (PackBytes a) where
  type Cardinality (PackBytes a) = Cardinality a
  {-# INLINE fromFinite #-}
  fromFinite = PackBytes . intoBytes
  {-# INLINE toFinite #-}
  toFinite = outOfBytes . op PackBytes

instance (Finitary a, 1 <= Cardinality a) => Bounded (PackBytes a) where
  {-# INLINE minBound #-}
  minBound = start
  {-# INLINE maxBound #-}
  maxBound = end

instance (Finitary a, 1 <= Cardinality a) => Storable (PackBytes a) where
  {-# INLINE sizeOf #-}
  sizeOf _ = byteLength @a
  {-# INLINE alignment #-}
  alignment _ = alignment (undefined :: Word8)
  {-# INLINE peek #-}
  peek ptr = do let bytePtr = castPtr ptr
                PackBytes <$> VU.generateM (byteLength @a) (peek . plusPtr bytePtr)
  {-# INLINE poke #-}
  poke ptr (PackBytes v) = do let bytePtr = castPtr ptr
                              VU.foldM'_ go bytePtr v
    where go p e = poke p e >> pure (plusPtr p 1)

newtype instance VU.MVector s (PackBytes a) = MV_PackBytes (VU.MVector s Word8)

instance (Finitary a, 1 <= Cardinality a) => VGM.MVector VU.MVector (PackBytes a) where
  {-# INLINE basicLength #-}
  basicLength = over MV_PackBytes ((`div` byteLength @a) . VGM.basicLength)
  {-# INLINE basicOverlaps #-}
  basicOverlaps = over2 MV_PackBytes VGM.basicOverlaps
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over MV_PackBytes (VGM.basicUnsafeSlice (i * byteLength @a) (len * byteLength @a))
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew len = MV_PackBytes <$> VGM.basicUnsafeNew (len * byteLength @a)
  {-# INLINE basicInitialize #-}
  basicInitialize = VGM.basicInitialize . op MV_PackBytes
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_PackBytes v) i = fmap PackBytes . VG.freeze . VGM.unsafeSlice (i * byteLength @a) (byteLength @a) $ v
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_PackBytes v) i (PackBytes x) = let slice = VGM.unsafeSlice (i * byteLength @a) (byteLength @a) v in
                                                        VG.unsafeCopy slice x

newtype instance VU.Vector (PackBytes a) = V_PackBytes (VU.Vector Word8)

instance (Finitary a, 1 <= Cardinality a) => VG.Vector VU.Vector (PackBytes a) where
  {-# INLINE basicLength #-}
  basicLength = over V_PackBytes ((`div` byteLength @a) . VG.basicLength)
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = fmap V_PackBytes . VG.basicUnsafeFreeze . op MV_PackBytes
  {-# INLINE basicUnsafeThaw #-} 
  basicUnsafeThaw = fmap MV_PackBytes . VG.basicUnsafeThaw . op V_PackBytes
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over V_PackBytes (VG.basicUnsafeSlice (i * byteLength @a) (len * byteLength @a))
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_PackBytes v) i = pure . PackBytes . VG.unsafeSlice (i * byteLength @a) (byteLength @a) $ v

instance (Finitary a, 1 <= Cardinality a) => VU.Unbox (PackBytes a)

-- Helpers

type ByteLength a = CLog (Cardinality Word8) (Cardinality a)

{-# INLINE byteLength #-}
byteLength :: forall (a :: Type) (b :: Type) . 
  (Finitary a, 1 <= Cardinality a, Num b) =>
  b
byteLength = fromIntegral . natVal $ (Proxy :: Proxy (ByteLength a)) 

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

{-# INLINE intoBytes #-}
intoBytes :: forall (n :: Nat) . 
  (KnownNat n, 1 <= n) => 
  Finite n -> VU.Vector Word8
intoBytes = evalState (VU.replicateM (byteLength @(Finite n)) go) . fromIntegral @_ @Natural
  where go = do remaining <- get
                let (d, r) = quotRem remaining 256
                put d >> pure (fromIntegral r)

{-# INLINE outOfBytes #-}
outOfBytes :: forall (n :: Nat) . 
  (KnownNat n) =>
  VU.Vector Word8 -> Finite n
outOfBytes v = evalState (VU.foldM' go 0 v) 1
  where go old w = do power <- get
                      let placeValue = power * fromIntegral w
                      modify (* 256)
                      return (old + placeValue) 
