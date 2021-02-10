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
-- Description:   Scheme for packing @Finitary@ types into @Word@ arrays.
-- Copyright:     (C) Koz Ross 2019
-- License:       GPL version 3.0 or later
-- Stability:     Experimental
-- Portability:   GHC only
--
-- If a type @a@ is 'Finitary', each inhabitant of @a@ has an index, which can
-- be represented as an unsigned integer, spread across one or more machine
-- words. This unsigned integer will have fixed length (as the number of
-- inhabitants of @a@ is finite). We can use this to derive a 'VU.Unbox'
-- instance, by representing 'VU.Vector' as a large array of machine words. We
-- can also derive a 'Storable' instance similarly.
--
-- This is the most efficient encoding of an arbitrary finitary type, both due
-- to the asymptotics of encoding and decoding (logarithmic in @Cardinality a@
-- with base @Cardinality Word@) and the fact that word accesses are faster than
-- byte and bit accesses on almost all architectures. Unless you have concerns
-- regarding space, this encoding is a good choice.
--
-- Unless your type's cardinality is extremely large (a non-trivial multiple of
-- @Cardinality Word@), this encoding is wasteful. If your type's cardinality is
-- smaller than that of @Word@, you should consider "Data.Finitary.PackInto"
-- instead, as you will have much larger control over space usage at almost no
-- performance penalty. 
module Data.Finitary.PackWords 
  ( -- * Packing and unpacking between a type and a little-endian array of 'Word's
    PackWords(.., Packed)

  -- * Helpers
  , intoWords, outOfWords
  )
  where

-- base
import Data.Kind (Type)
import Data.Hashable (Hashable(..), hashByteArrayWithSalt)
import Foreign.Storable (Storable(..))
import GHC.Exts
import GHC.IO
import GHC.Natural (Natural(..))
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
-- ghc-bignum
import GHC.Num.BigNat (BigNat(..), bigNatCompare, bigNatSize#)
import GHC.Num.Integer (integerToNaturalClamp, integerFromBigNat#)
#else
-- integer-gmp
import GHC.Integer.GMP.Internals
  ( BigNat(..), bigNatToInteger, compareBigNat, sizeofBigNat# )
#endif

--------------------------------------------------------------------------------

-- | An opaque wrapper around @a@, representing each value as a fixed-length
-- array of machine words.
newtype PackWords (a :: Type) = PackedWords ByteArray
  deriving (Eq, Show)

type role PackWords nominal

-- | To provide (something that resembles a) data constructor for 'PackWords', we
-- provide the following pattern. It can be used like any other data
-- constructor:
--
-- > import Data.Finitary.PackWords
-- >
-- > anInt :: PackWords Int
-- > anInt = Packed 10
-- >
-- > isPackedEven :: PackWords Int -> Bool
-- > isPackedEven (Packed x) = even x
--
-- __Every__ pattern match, and data constructor call, performs a
-- \(\Theta(\log_{\texttt{Cardinality Word}}(\texttt{Cardinality a}))\) encoding or decoding of @a@.
-- Use with this in mind.
{-# COMPLETE Packed #-}
pattern Packed :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  a -> PackWords a
pattern Packed x <- (unpackWords -> x)
  where Packed x = packWords x

instance (Finitary a, 1 <= Cardinality a) => Ord (PackWords a) where
  compare (PackedWords (ByteArray ba1)) (PackedWords (ByteArray ba2)) =
#ifdef BIGNUM
    bigNatCompare ba1 ba2
#else
    compareBigNat (BN# ba1) (BN# ba2)
#endif

-- Re-use primitive vector instance for 'Binary'.
instance (Finitary a, 1 <= Cardinality a) => Bin.Binary (PackWords a) where
  {-# INLINE put #-}
  put = Bin.put . VP.Vector @Word 0 (wordLength @a) . op PackedWords
  {-# INLINE get #-}
  get = PackedWords . ( \ ( VP.Vector _ _ ba :: VP.Vector Word ) -> ba ) <$> Bin.get

instance (Finitary a, 1 <= Cardinality a) => Hashable (PackWords a) where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt = ( \ ( ByteArray ba ) -> hashByteArrayWithSalt ba 0 (bytesPerWord * wordLength @a) salt )
                    . op PackedWords

instance NFData (PackWords a) where
  {-# INLINE rnf #-}
  rnf = rnf . op PackedWords

instance (Finitary a, 1 <= Cardinality a) => Finitary (PackWords a) where
  type Cardinality (PackWords a) = Cardinality a
  {-# INLINE fromFinite #-}
  fromFinite = PackedWords . intoWords
  {-# INLINE toFinite #-}
  toFinite = outOfWords . op PackedWords

instance (Finitary a, 1 <= Cardinality a) => Bounded (PackWords a) where
  {-# INLINE minBound #-}
  minBound = start
  {-# INLINE maxBound #-}
  maxBound = end

instance (Finitary a, 1 <= Cardinality a) => Storable (PackWords a) where
  {-# INLINABLE sizeOf #-}
  sizeOf _ = wordLength @a * bytesPerWord
  {-# INLINABLE alignment #-}
  alignment _ = alignment (undefined :: Word)
  {-# INLINABLE peek #-}
  peek (Ptr addr) =
    IO $ \ s1 ->
      case newByteArray# nbBytes s1 of
        (# s2, mba #) -> case copyAddrToByteArray# addr mba 0# nbBytes s2 of
          s3 -> case unsafeFreezeByteArray# mba s3 of
            (# s4, ba #) -> (# s4, PackedWords (ByteArray ba) #)
    where
      nbBytes :: Int#
      !(I# nbBytes) = bytesPerWord * wordLength @a
  {-# INLINE poke #-}
  poke (Ptr addr) (PackedWords (ByteArray ba)) =
    IO $ \ s1 ->
      case copyByteArrayToAddr# ba 0# addr nbBytes s1 of
        s2 -> (# s2, () #)
    where
      nbBytes :: Int#
      !(I# nbBytes) = bytesPerWord * wordLength @a

newtype instance VU.MVector s (PackWords a) = MV_PackWords (VU.MVector s Word)

instance (Finitary a, 1 <= Cardinality a) => VGM.MVector VU.MVector (PackWords a) where
  {-# INLINE basicLength #-}
  basicLength = over MV_PackWords ((`div` wordLength @a) . VGM.basicLength)
  {-# INLINE basicOverlaps #-}
  basicOverlaps = over2 MV_PackWords VGM.basicOverlaps
  {-# INLINABLE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over MV_PackWords (VGM.basicUnsafeSlice (i * wordLength @a) (len * wordLength @a))
  {-# INLINABLE basicUnsafeNew #-}
  basicUnsafeNew len = MV_PackWords <$> VGM.basicUnsafeNew (len * wordLength @a)
  {-# INLINE basicInitialize #-}
  basicInitialize = VGM.basicInitialize . op MV_PackWords
  {-# INLINABLE basicUnsafeRead #-}
  basicUnsafeRead (MV_PackWords (VU.MV_Word (VPM.MVector (I# off) _ (MutableByteArray full_mba)))) (I# i) =
    primitive $ \ s1 ->
      case newByteArray# nbBytes s1 of
        (# s2, elem_mba #) -> case copyMutableByteArray# full_mba (off +# nbBytes *# i) elem_mba 0# nbBytes s2 of
          s3 -> case unsafeFreezeByteArray# elem_mba s3 of
            (# s4, elem_ba #) -> (# s4, PackedWords (ByteArray elem_ba) #)
    where
      nbBytes :: Int#
      !(I# nbBytes) = bytesPerWord * wordLength @a
  {-# INLINABLE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_PackWords (VU.MV_Word (VPM.MVector (I# off) _ (MutableByteArray full_mba)))) (I# i) (PackedWords (ByteArray val_ba)) =
    primitive $ \ s1 -> case copyByteArray# val_ba 0# full_mba (off +# nbBytes *# i) nbBytes s1 of
      s2 -> (# s2, () #)
      where
        nbBytes :: Int#
        !(I# nbBytes) = bytesPerWord * wordLength @a

newtype instance VU.Vector (PackWords a) = V_PackWords (VU.Vector Word)

instance (Finitary a, 1 <= Cardinality a) => VG.Vector VU.Vector (PackWords a) where
  {-# INLINE basicLength #-}
  basicLength = over V_PackWords ((`div` wordLength @a) . VG.basicLength)
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = fmap V_PackWords . VG.basicUnsafeFreeze . op MV_PackWords
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = fmap MV_PackWords . VG.basicUnsafeThaw . op V_PackWords
  {-# INLINABLE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over V_PackWords (VG.basicUnsafeSlice (i * wordLength @a) (len * wordLength @a))
  {-# INLINABLE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_PackWords (VU.V_Word (VP.Vector (I# off) _ (ByteArray full_ba)))) (I# i) =
    pure $ runRW# $ \ s1 ->
      case newByteArray# nbBytes s1 of
        (# s2, elem_mba #) -> case copyByteArray# full_ba (off +# nbBytes *# i) elem_mba 0# nbBytes s2 of
          s3 -> case unsafeFreezeByteArray# elem_mba s3 of
            (# _, elem_ba #) -> PackedWords (ByteArray elem_ba)
    where
      nbBytes :: Int#
      !(I# nbBytes) = bytesPerWord * wordLength @a

instance (Finitary a, 1 <= Cardinality a) => VU.Unbox (PackWords a)

-- Helpers

type WordLength a = NatWords (Cardinality a)
type NatWords n = CLog (Cardinality Word) n

{-# INLINE bytesPerWord #-}
bytesPerWord :: forall (a :: Type) . 
  (Num a) => 
  a
bytesPerWord = fromIntegral . sizeOf $ (undefined :: Word)

{-# INLINE wordLength #-}
wordLength :: forall (a :: Type) (b :: Type) . 
  (Finitary a, 1 <= Cardinality a, Num b) => 
  b
wordLength = fromIntegral $ natVal' @(WordLength a) proxy#

{-# INLINE natWords #-}
natWords :: forall (n :: Nat) (b :: Type) .
  (KnownNat n, 1 <= n, Num b) =>
  b
natWords = fromIntegral $ natVal' @(NatWords n) proxy#

{-# INLINE packWords #-}
packWords :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  a -> PackWords a
packWords = fromFinite . toFinite

{-# INLINE unpackWords #-}
unpackWords :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  PackWords a -> a
unpackWords = fromFinite . toFinite

{-# INLINABLE intoWords #-}
intoWords :: forall (n :: Nat) . 
  (KnownNat n, 1 <= n) => 
  Finite n -> ByteArray
intoWords f = runRW# $ \ s1 ->
  case newByteArray# nbBytes s1 of
    (# s2, mba #) ->
      case (
        case i of
          NatS# word
            | 0## <- word
            -> (# s2, 0# #)
            | otherwise
            -> case writeWordArray# mba 0# word s2 of
                s3 -> (# s3, wordSize #)
          NatJ# (BN# bigNatArray) ->
            let
              nbBytesWritten :: Int#
#ifdef BIGNUM
              nbBytesWritten = wordSize *# bigNatSize# bigNatArray
#else
              nbBytesWritten = wordSize *# sizeofBigNat# (BN# bigNatArray)
#endif
            in
              case copyByteArray# bigNatArray 0# mba 0# nbBytesWritten s2 of
                s3 -> (# s3, nbBytesWritten #)
        ) of
        (# s3, bytesWritten #) ->
          case setByteArray# mba bytesWritten (nbBytes -# bytesWritten) 0# s3 of
            s4 -> case unsafeFreezeByteArray# mba s4 of
              (# _, ba #) -> ByteArray ba

  where
    wordSize :: Int#
    !(I# wordSize) = bytesPerWord
    nbBytes :: Int#
    !(I# nbBytes) = I# wordSize * natWords @n
    i :: Natural
    i =
#ifdef BIGNUM
      integerToNaturalClamp ( getFinite f )
#else
      fromIntegral ( getFinite f )
#endif

{-# INLINABLE outOfWords #-}
outOfWords :: forall (n :: Nat) .
  (KnownNat n) => 
  ByteArray -> Finite n
outOfWords (ByteArray ba) =
#ifdef BIGNUM
  Finite $ integerFromBigNat# ba
#else
  Finite $ bigNatToInteger (BN# ba)
#endif
