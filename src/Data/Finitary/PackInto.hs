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

{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module:        Data.Finitary.PackInto
-- Description:   Scheme for packing @Finitary@ types into other @Finitary@
--                types.
-- Copyright:     (C) Koz Ross 2019
-- License:       GPL version 3.0 or later
-- Stability:     Experimental
-- Portability:   GHC only
--
-- This allows us to \'borrow\' implementations of certain type classes from
-- \'larger\' finitary types for \'smaller\' finitary types. Essentially, for
-- any types @a@ and @b@, if both @a@ and @b@ are 'Finitary' and @Cardinality a
-- <= Cardinality b@, the set of indexes for @a@ is a subset (strictly speaking,
-- a prefix) of the set of indexes for @b@. Therefore, we have an injective
-- mapping from @a@ to @b@, whose
-- [preimage](https://en.wikipedia.org/wiki/Preimage)
-- is also injective, witnessed by the function @fromFinite . toFinite@ in both
-- directions. When combined with the monotonicity of @toFinite@ and
-- @fromFinite@, we can operate on inhabitants of @b@ in certain ways while
-- always being able to recover the \'equivalent\' inhabitant of @a@.
--
-- On this basis, we can \'borrow\' both 'VU.Unbox' and 'Storable' instances
-- from @b@. This is done by way of the @PackInto a b@ type; here, @a@ is the
-- type to which instances are being \'lent\' and @b@ is the type from which
-- instances are being \'borrowed\'. @PackInto a b@ does not store any values of
-- type @a@ - construction and deconstruction of @PackInto@ performs a
-- conversion as described above.
--
-- If an existing 'Finitary' type exists with desired instances, this encoding
-- is the most flexible and efficient. Unless you have good reasons to consider
-- something else (such as space use), use this encoding. However, its
-- usefulness is conditional on a suitable \'packing\' type existing of
-- appropriate cardinality. Additionally, if @Cardinality a < Cardinality b@,
-- any @PackInto a b@ will waste some space, with larger cardinality differences
-- creating proportionately more waste.
module Data.Finitary.PackInto 
(
  PackInto, pattern Packed
) where

import GHC.TypeNats
import Data.Vector.Instances ()
import Data.Kind (Type)
import CoercibleUtils (op, over, over2)
import Data.Hashable (Hashable(..))
import Control.DeepSeq (NFData(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr)
import Data.Finitary (Finitary(..))
import Data.Finite (weakenN, strengthenN)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

-- | An opaque wrapper, representing values of type @a@ as \'corresponding\'
-- values of type @b@.
newtype PackInto (a :: Type) (b :: Type) = PackInto b
  deriving (Eq, Show)

type role PackInto nominal nominal

-- | To provide (something that resembles a) data constructor for 'PackInto', we
-- provide the following pattern. It can be used like any other data
-- constructor:
--
-- > import Data.Finitary.PackInt
-- >
-- > anInt :: PackInto Int Word
-- > anInt = Packed 10
-- >
-- > isPackedEven :: PackInto Int Word -> Bool
-- > isPackedEven (Packed x) = even x
--
-- __Every__ pattern match, and data constructor call, performs a re-encoding by
-- way of @fromFinite . toFinite@ on @b@ and @a@ respectively. Use with this in
-- mind.
{-# COMPLETE Packed #-}
pattern Packed :: forall (b :: Type) (a :: Type) . 
  (Finitary a, Finitary b, Cardinality a <= Cardinality b) =>
  a -> PackInto a b
pattern Packed x <- (unpackOutOf -> x)
  where Packed x = packInto x

instance (Ord a, Finitary a, Finitary b, Cardinality a <= Cardinality b) => Ord (PackInto a b) where
  {-# INLINE compare #-}
  compare = comparing @a (fromFinite . toFinite)

instance (Hashable b) => Hashable (PackInto a b) where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt = over PackInto (hashWithSalt salt)

instance (NFData b) => NFData (PackInto a b) where
  {-# INLINE rnf #-}
  rnf = over PackInto rnf

instance (Storable b) => Storable (PackInto a b) where
  {-# INLINE sizeOf #-}
  sizeOf = over PackInto sizeOf
  {-# INLINE alignment #-}
  alignment = over PackInto alignment
  {-# INLINE peek #-}
  peek = fmap PackInto . peek . castPtr
  {-# INLINE poke #-}
  poke ptr = poke (castPtr ptr) . op PackInto

-- We can pack a into b if the cardinality of b is at least as large as a (could
-- be larger)
instance (Finitary a, Finitary b, Cardinality a <= Cardinality b) => Finitary (PackInto a b) where
  type Cardinality (PackInto a b) = Cardinality a
  {-# INLINE fromFinite #-}
  fromFinite = PackInto . fromFinite . weakenN
  {-# INLINE toFinite #-}
  toFinite = fromJust . strengthenN . toFinite . op PackInto

instance (Finitary a, Finitary b, 1 <= Cardinality a, Cardinality a <= Cardinality b) => Bounded (PackInto a b) where
  {-# INLINE minBound #-}
  minBound = start
  {-# INLINE maxBound #-}
  maxBound = end 

newtype instance VU.MVector s (PackInto a b) = MV_PackInto (VU.MVector s b)

instance (VU.Unbox b) => VGM.MVector VU.MVector (PackInto a b) where
  {-# INLINE basicLength #-}
  basicLength = over MV_PackInto VGM.basicLength
  {-# INLINE basicOverlaps #-}
  basicOverlaps = over2 MV_PackInto VGM.basicOverlaps
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over MV_PackInto (VGM.basicUnsafeSlice i len)
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew len = MV_PackInto <$> VGM.basicUnsafeNew len
  {-# INLINE basicInitialize #-}
  basicInitialize = VGM.basicInitialize . op MV_PackInto
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_PackInto v) i = PackInto <$> VGM.basicUnsafeRead v i
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_PackInto v) i (PackInto x) = VGM.basicUnsafeWrite v i x

newtype instance VU.Vector (PackInto a b) = V_PackInto (VU.Vector b)

instance (VU.Unbox b) => VG.Vector VU.Vector (PackInto a b) where
  {-# INLINE basicLength #-}
  basicLength = over V_PackInto VG.basicLength
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = fmap V_PackInto . VG.basicUnsafeFreeze . op MV_PackInto
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = fmap MV_PackInto . VG.basicUnsafeThaw . op V_PackInto
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over V_PackInto (VG.basicUnsafeSlice i len)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_PackInto v) i = PackInto <$> VG.basicUnsafeIndexM v i

instance (VU.Unbox b) => VU.Unbox (PackInto a b)

-- Helpers

{-# INLINE packInto #-}
packInto :: forall (b :: Type) (a :: Type) .
  (Finitary a, Finitary b, Cardinality a <= Cardinality b) =>  
  a -> PackInto a b
packInto = fromFinite . toFinite

{-# INLINE unpackOutOf #-}
unpackOutOf :: forall (b :: Type) (a :: Type) . 
  (Finitary a, Finitary b, Cardinality a <= Cardinality b) => 
  PackInto a b -> a
unpackOutOf = fromFinite . toFinite
