{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Finitary.PackInto where

import Data.Maybe (fromJust)
import CoercibleUtils (op, over, over2)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Data.Finitary (Finitary(..))
import GHC.TypeNats
import Data.Finite (weakenN, strengthenN)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

newtype PackInto (a :: Type) (b :: Type) = PackInto { unpackFrom :: b }
  deriving (Eq, Ord, Bounded, Generic)

instance (Finitary b) => Finitary (PackInto a b)

newtype instance VU.MVector s (PackInto a b) = MV_PackInto (VU.MVector s a)

instance (Finitary a, Finitary b, Cardinality b <= Cardinality a, VU.Unbox a) => VGM.MVector VU.MVector (PackInto a b) where
  {-# INLINE basicLength #-}
  basicLength = VGM.basicLength . op MV_PackInto
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over MV_PackInto (VGM.basicUnsafeSlice i len)
  {-# INLINE basicOverlaps #-}
  basicOverlaps = over2 MV_PackInto VGM.basicOverlaps
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew = fmap MV_PackInto . VGM.basicUnsafeNew
  {-# INLINE basicInitialize #-}
  basicInitialize = VGM.basicInitialize . op MV_PackInto
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate len = fmap MV_PackInto . VGM.basicUnsafeReplicate len . fromFinite . weakenN . toFinite . unpackFrom
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_PackInto v) = fmap (PackInto . fromFinite . fromJust . strengthenN . toFinite) . VGM.basicUnsafeRead v
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_PackInto v) i = VGM.basicUnsafeWrite v i . fromFinite . weakenN . toFinite . unpackFrom
  {-# INLINE basicClear #-}
  basicClear = VGM.basicClear . op MV_PackInto
  {-# INLINE basicSet #-}
  basicSet (MV_PackInto v) = VGM.basicSet v . fromFinite . weakenN . toFinite . unpackFrom
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_PackInto dst) (MV_PackInto src) = VGM.basicUnsafeCopy dst src
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MV_PackInto dst) (MV_PackInto src) = VGM.basicUnsafeMove dst src
  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow (MV_PackInto v) = fmap MV_PackInto . VGM.basicUnsafeGrow v

newtype instance VU.Vector (PackInto a b) = V_PackInto (VU.Vector a)

instance (Finitary a, Finitary b, Cardinality b <= Cardinality a, VU.Unbox a) => VG.Vector VU.Vector (PackInto a b) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = fmap V_PackInto . VG.basicUnsafeFreeze . op MV_PackInto
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = fmap MV_PackInto . VG.basicUnsafeThaw . op V_PackInto
  {-# INLINE basicLength #-}
  basicLength = over V_PackInto VG.basicLength
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over V_PackInto (VG.basicUnsafeSlice i len)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_PackInto v) = fmap (PackInto . fromFinite . fromJust . strengthenN . toFinite) . VG.basicUnsafeIndexM v
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_PackInto dst) (V_PackInto src) = VG.basicUnsafeCopy dst src

instance (Finitary a, Finitary b, Cardinality b <= Cardinality a, VU.Unbox a) => VU.Unbox (PackInto a b)
