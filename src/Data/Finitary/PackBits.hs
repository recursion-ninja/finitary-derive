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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

#ifndef BITVEC_UNSAFE
-- |
-- Module:        Data.Finitary.PackBits
-- Description:   Scheme for bit-packing @Finitary@ types.
-- Copyright:     (C) Koz Ross 2019
-- License:       GPL version 3.0 or later
-- Stability:     Experimental
-- Portability:   GHC only
--
-- From the [Kraft-McMillan
-- inequality](https://en.wikipedia.org/wiki/Kraft%E2%80%93McMillan_inequality)
-- and 
-- the fact that we are not able to have \'fractional\' bits, we can derive a
-- fixed-length code into a bitstring for any 'Finitary' type @a@, with code
-- length \(\lceil \log_{2}(\texttt{Cardinality a}) \rceil\) bits. This code is
-- essentially a binary representation of the index of each inhabitant of @a@.
-- On that basis, we can derive an 'VU.Unbox' instance, representing
-- the entire 'VU.Vector' as an unboxed [bit
-- array](https://en.wikipedia.org/wiki/Bit_array).
--
-- This encoding is advantageous from the point of view of space - there is no
-- tighter possible packing that preserves \(\Theta(1)\) random access and also
-- allows the full range of 'VU.Vector' operations. If you are concerned about
-- space usage above all, this is the best choice for you. 
--
-- Because access to individual bits is slower than whole bytes or words, this
-- encoding adds some overhead. Additionally, a primary advantage of bit arrays
-- (the ability to perform \'bulk\' operations on bits efficiently) is not made
-- use of here. Therefore, if speed matters more than compactness, this encoding
-- is suboptimal.
--
-- This encoding is __thread-safe__, and thus slightly slower. If you are certain 
-- that race conditions cannot occur for your code, you can gain a speed improvement 
-- by using "Data.Finitary.PackBits.Unsafe" instead.

module Data.Finitary.PackBits 
#else
-- |
-- Module:        Data.Finitary.PackBits.Unsafe
-- Description:   Scheme for bit-packing @Finitary@ types.
-- Copyright:     (C) Koz Ross 2019
-- License:       GPL version 3.0 or later
-- Stability:     Experimental
-- Portability:   GHC only
--
-- From the [Kraft-McMillan
-- inequality](https://en.wikipedia.org/wiki/Kraft%E2%80%93McMillan_inequality)
-- and 
-- the fact that we are not able to have \'fractional\' bits, we can derive a
-- fixed-length code into a bitstring for any 'Finitary' type @a@, with code
-- length \(\lceil \log_{2}(\texttt{Cardinality a}) \rceil\) bits. This code is
-- essentially a binary representation of the index of each inhabitant of @a@.
-- On that basis, we can derive an 'VU.Unbox' instance, representing
-- the entire 'VU.Vector' as an unboxed [bit
-- array](https://en.wikipedia.org/wiki/Bit_array).
--
-- This encoding is advantageous from the point of view of space - there is no
-- tighter possible packing that preserves \(\Theta(1)\) random access and also
-- allows the full range of 'VU.Vector' operations. If you are concerned about
-- space usage above all, this is the best choice for you. 
--
-- Because access to individual bits is slower than whole bytes or words, this
-- encoding adds some overhead. Additionally, a primary advantage of bit arrays
-- (the ability to perform \'bulk\' operations on bits efficiently) is not made
-- use of here. Therefore, if speed matters more than compactness, this encoding
-- is suboptimal.
--
-- This encoding is __not__ thread-safe, in exchange for performance. If you
-- suspect race conditions are possible, it's better to use
-- "Data.Finitary.PackBits" instead.
module Data.Finitary.PackBits.Unsafe
#endif
  ( -- * Packing and unpacking between a type and a bit vector
    PackBits(PackedBits, Packed)
  , BulkPack, exposeVector
  
  -- * Helpers
  , intoBits, outOfBits
  )
where

import Data.Finitary.Coercion (op, over, over2)

-- base
import Data.Kind (Type)
import Data.Hashable (Hashable(..))
import GHC.Exts
import GHC.TypeNats

-- binary
import qualified Data.Binary as Bin

-- bitvec
#ifndef BITVEC_UNSAFE
import qualified Data.Bit.ThreadSafe as BV
#else
import qualified Data.Bit as BV
#endif

-- deepseq
import Control.DeepSeq (NFData(..))

-- finitary
import Data.Finitary (Finitary(..))

-- finitary-derive
import Data.Finitary.PackWords
  ( PackWords(PackedWords), intoWords, outOfWords )

-- finite-typelits
import Data.Finite.Internal (Finite(..))

-- ghc-typelits-extra
import GHC.TypeLits.Extra

-- primitive
import Data.Primitive.ByteArray (ByteArray(..))

-- vector
import qualified Data.Vector.Unboxed.Base    as VU
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM

-- vector-binary-instances
import Data.Vector.Binary ()

-- vector-instances
import Data.Vector.Instances ()

--------------------------------------------------------------------------------

-- | An opaque wrapper around @a@, representing each value as a 'bit-packed'
-- encoding.
newtype PackBits (a :: Type) = PackedBits (VU.Vector BV.Bit)
  deriving (Eq, Show)

type role PackBits nominal

-- | To provide (something that resembles a) data constructor for 'PackBits', we
-- provide the following pattern. It can be used like any other data
-- constructor:
--
-- > import Data.Finitary.PackBits
-- >
-- > anInt :: PackBits Int
-- > anInt = Packed 10
-- >
-- > isPackedEven :: PackBits Int -> Bool
-- > isPackedEven (Packed x) = even x
--
-- __Every__ pattern match, and data constructor call, performs a
-- \(\Theta(\log_{2}(\texttt{Cardinality a}))\) encoding or decoding operation. 
-- Use with this in mind.
{-# COMPLETE Packed #-}
pattern Packed :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  a -> PackBits a
pattern Packed x <- (unpackBits -> x)
  where Packed x = packBits x

instance (Finitary a, 1 <= Cardinality a) => Ord (PackBits a) where
  {-# INLINABLE compare #-}
  compare (PackedBits (BV.BitVec _ _ v1)) (PackedBits (BV.BitVec _ _ v2)) =
    compare (PackedWords v1 :: PackWords a) (PackedWords v2 :: PackWords a)

instance NFData (PackBits a) where
  {-# INLINE rnf #-}
  rnf = rnf . op PackedBits

instance (Finitary a, 1 <= Cardinality a) => Finitary (PackBits a) where
  type Cardinality (PackBits a) = Cardinality a
  {-# INLINE fromFinite #-}
  fromFinite = PackedBits . intoBits
  {-# INLINE toFinite #-}
  toFinite = outOfBits . op PackedBits

instance (Finitary a, 1 <= Cardinality a) => Bounded (PackBits a) where
  {-# INLINE minBound #-}
  minBound = start
  {-# INLINE maxBound #-}
  maxBound = end

newtype instance VU.MVector s (PackBits a) = MV_PackBits (VU.MVector s BV.Bit)

instance (Finitary a, 1 <= Cardinality a) => VGM.MVector VU.MVector (PackBits a) where
  {-# INLINE basicLength #-}
  basicLength = over MV_PackBits ((`div` bitLength @a) . VGM.basicLength)
  {-# INLINE basicOverlaps #-}
  basicOverlaps = over2 MV_PackBits VGM.basicOverlaps
  {-# INLINABLE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over MV_PackBits (VGM.basicUnsafeSlice (i * bitLength @a) (len * bitLength @a))
  {-# INLINABLE basicUnsafeNew #-}
  basicUnsafeNew len = fmap MV_PackBits (VGM.basicUnsafeNew (len * bitLength @a))
  {-# INLINE basicInitialize #-}
  basicInitialize = VGM.basicInitialize . op MV_PackBits
  {-# INLINABLE basicUnsafeRead #-}
  basicUnsafeRead (MV_PackBits v) i = fmap PackedBits . VG.freeze . VGM.unsafeSlice (i * bitLength @a) (bitLength @a) $ v
  {-# INLINABLE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_PackBits v) i (PackedBits x) = let slice = VGM.unsafeSlice (i * bitLength @a) (bitLength @a) v in
                                                      VG.unsafeCopy slice x

newtype instance VU.Vector (PackBits a) = V_PackBits (VU.Vector BV.Bit)

instance (Finitary a, 1 <= Cardinality a) => VG.Vector VU.Vector (PackBits a) where
  {-# INLINE basicLength #-}
  basicLength = over V_PackBits ((`div` bitLength @a) . VG.basicLength)
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = fmap V_PackBits . VG.basicUnsafeFreeze . op MV_PackBits
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = fmap MV_PackBits . VG.basicUnsafeThaw . op V_PackBits
  {-# INLINABLE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over V_PackBits (VG.basicUnsafeSlice (i * bitLength @a) (len * bitLength @a))
  {-# INLINABLE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_PackBits v) i = pure . PackedBits . VG.unsafeSlice (i * bitLength @a) (bitLength @a) $ v

instance (Finitary a, 1 <= Cardinality a) => VU.Unbox (PackBits a)

-- | This wrapper provides an efficient 'Hashable' instance (hash the entire
-- underlying bit-packed vector, rather than each element individually), as well
-- as a 'Bin.Binary' instance (which stores or reads the entire blob of
-- bits \'in one go\').
newtype BulkPack a = BulkPack { exposeVector :: VU.Vector (PackBits a) }
  deriving (NFData)

deriving instance (Finitary a, 1 <= Cardinality a) => Eq (BulkPack a)

deriving instance (Finitary a, 1 <= Cardinality a) => Ord (BulkPack a)

instance (Finitary a, 1 <= Cardinality a) => Hashable (BulkPack a) where
  {-# INLINABLE hashWithSalt #-}
  hashWithSalt salt = hashWithSalt salt . BV.cloneToWords . op V_PackBits . op BulkPack

instance Bin.Binary (BulkPack a) where
  {-# INLINE put #-}
  put = Bin.put . BV.cloneToWords . op V_PackBits . op BulkPack
  {-# INLINE get #-}
  get = BulkPack . V_PackBits . BV.castFromWords <$> Bin.get

-- Helpers

type BitLength a = CLog 2 (Cardinality a)

{-# INLINE packBits #-}
packBits :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  a -> PackBits a
packBits = fromFinite . toFinite

{-# INLINE unpackBits #-}
unpackBits :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  PackBits a -> a
unpackBits = fromFinite . toFinite

{-# INLINE bitLength #-}
bitLength :: forall (a :: Type) (b :: Type) . 
  (Finitary a, 1 <= Cardinality a, Num b) => 
  b
bitLength = fromIntegral $ natVal' @(BitLength a) proxy#

{-# INLINABLE intoBits #-}
intoBits :: forall (n :: Nat) .
  (KnownNat n, 1 <= n) =>  
  Finite n -> VU.Vector BV.Bit
intoBits f = BV.BitVec 0 nbBits wordArray
  where
    wordArray :: ByteArray
    wordArray = intoWords f
    nbBits :: Int
    nbBits = fromIntegral $ natVal' @( CLog 2 n ) proxy#

{-# INLINABLE outOfBits #-}
outOfBits :: forall (n :: Nat) .
  (KnownNat n) =>  
  VU.Vector BV.Bit -> Finite n
outOfBits (BV.BitVec _ _ wordArray) = outOfWords @n wordArray
