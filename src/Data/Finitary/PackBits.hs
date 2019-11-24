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

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module:        Data.Finitary.PackBits
-- Description:   Scheme for bit-packing @Finitary@ types.
-- Copyright:     (C) Koz Ross 2019
-- License:       GPL version 3.0 or later
-- Maintainer:    koz.ross@retro-freedom.nz
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
(
  PackBits, pattern Packed,
  BulkPack, exposeVector
) where

import GHC.TypeLits.Extra
import Data.Proxy (Proxy(..))
import Numeric.Natural (Natural)
import GHC.TypeNats
import CoercibleUtils (op, over, over2)
import Data.Kind (Type)
import Data.Hashable (Hashable(..))
import Data.Vector.Instances ()
import Data.Vector.Binary ()
import Control.DeepSeq (NFData(..))
import Data.Finitary(Finitary(..))
import Data.Finite (Finite)
import Control.Monad.Trans.State.Strict (evalState, get, modify, put)
import Data.Semigroup (Dual(..))

import qualified Data.Binary as Bin
import qualified Data.Bit.ThreadSafe as BT
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

-- | An opaque wrapper around @a@, representing each value as a 'bit-packed'
-- encoding.
newtype PackBits (a :: Type) = PackBits (VU.Vector BT.Bit)
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
pattern Packed :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  PackBits a -> a
pattern Packed x <- (packBits -> x)
  where Packed x = unpackBits x

instance Ord (PackBits a) where
  compare (PackBits v1) (PackBits v2) = getDual . VU.foldr go (Dual EQ) . VU.zipWith (,) v1 $ v2
    where go input order = (order <>) . Dual . uncurry compare $ input

instance NFData (PackBits a) where
  {-# INLINE rnf #-}
  rnf = rnf . op PackBits

instance (Finitary a, 1 <= Cardinality a) => Finitary (PackBits a) where
  type Cardinality (PackBits a) = Cardinality a
  {-# INLINE fromFinite #-}
  fromFinite = PackBits . intoBits
  {-# INLINE toFinite #-}
  toFinite = outOfBits . op PackBits

instance (Finitary a, 1 <= Cardinality a) => Bounded (PackBits a) where
  {-# INLINE minBound #-}
  minBound = start
  {-# INLINE maxBound #-}
  maxBound = end

newtype instance VU.MVector s (PackBits a) = MV_PackBits (VU.MVector s BT.Bit)

instance (Finitary a, 1 <= Cardinality a) => VGM.MVector VU.MVector (PackBits a) where
  {-# INLINE basicLength #-}
  basicLength = over MV_PackBits ((`div` bitLength @a) . VGM.basicLength)
  {-# INLINE basicOverlaps #-}
  basicOverlaps = over2 MV_PackBits VGM.basicOverlaps
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over MV_PackBits (VGM.basicUnsafeSlice (i * bitLength @a) (len * bitLength @a))
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew len = fmap MV_PackBits (VGM.basicUnsafeNew (len * bitLength @a))
  {-# INLINE basicInitialize #-}
  basicInitialize = VGM.basicInitialize . op MV_PackBits
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_PackBits v) i = fmap PackBits . VG.freeze . VGM.unsafeSlice (i * bitLength @a) (bitLength @a) $ v
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_PackBits v) i (PackBits x) = let slice = VGM.unsafeSlice (i * bitLength @a) (bitLength @a) v in
                                                      VG.unsafeCopy slice x

newtype instance VU.Vector (PackBits a) = V_PackBits (VU.Vector BT.Bit)

instance (Finitary a, 1 <= Cardinality a) => VG.Vector VU.Vector (PackBits a) where
  {-# INLINE basicLength #-}
  basicLength = over V_PackBits ((`div` bitLength @a) . VG.basicLength)
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = fmap V_PackBits . VG.basicUnsafeFreeze . op MV_PackBits
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = fmap MV_PackBits . VG.basicUnsafeThaw . op V_PackBits
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over V_PackBits (VG.basicUnsafeSlice (i * bitLength @a) (len * bitLength @a))
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_PackBits v) i = pure . PackBits . VG.unsafeSlice (i * bitLength @a) (bitLength @a) $ v

instance (Finitary a, 1 <= Cardinality a) => VU.Unbox (PackBits a)

-- | This wrapper provides an efficient 'Hashable' instance (hash the entire
-- underlying bit-packed vector, rather than each element individually), as well
-- as a 'Bin.Binary' instance (which stores or reads the entire blob of
-- bits \'in one go\').
newtype BulkPack a = BulkPack { exposeVector :: VU.Vector (PackBits a) }
  deriving (NFData)

deriving instance (Finitary a, 1 <= Cardinality a) => Eq (BulkPack a)

deriving instance (Finitary a, 1 <= Cardinality a) => Ord (BulkPack a)

instance Hashable (BulkPack a) where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt = hashWithSalt salt . BT.cloneToWords . op V_PackBits . op BulkPack

instance Bin.Binary (BulkPack a) where
  {-# INLINE put #-}
  put = Bin.put . BT.cloneToWords . op V_PackBits . op BulkPack
  {-# INLINE get #-}
  get = BulkPack . V_PackBits . BT.castFromWords <$> Bin.get

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
bitLength = fromIntegral . natVal $ (Proxy :: Proxy (BitLength a))

{-# INLINE intoBits #-}
intoBits :: forall (n :: Nat) .
  (KnownNat n, 1 <= n) =>  
  Finite n -> VU.Vector BT.Bit
intoBits = evalState (VU.replicateM (bitLength @(Finite n)) go) . fromIntegral @_ @Natural
  where go = do remaining <- get
                let (d, r) = quotRem remaining 2
                put d >> pure (BT.Bit . toEnum . fromIntegral $ r)
                
{-# INLINE outOfBits #-}
outOfBits :: forall (n :: Nat) .
  (KnownNat n) =>  
  VU.Vector BT.Bit -> Finite n
outOfBits v = evalState (VU.foldM' go 0 v) 1
  where go old (BT.Bit b) = do power <- get
                               let placeValue = power * (fromIntegral . fromEnum $ b)
                               modify (* 2)
                               return (old + placeValue)
