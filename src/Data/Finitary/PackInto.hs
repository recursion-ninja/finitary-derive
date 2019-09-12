{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Finitary.PackInto 
(
  PackInto(..)
) where

import Control.Applicative (empty)
import Data.Proxy (Proxy(..))
import Data.Maybe (fromJust)
import CoercibleUtils (op, over, over2)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Data.Finitary (Finitary(..))
import GHC.TypeNats
import Data.Finite (weakenN, strengthenN)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr)
import Type.Reflection (Typeable)
import Data.Binary (Binary(..))

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

-- p is the 'pack into' type
newtype PackInto (p :: Type) (a :: Type) = PackInto { unpackFrom :: a }
  deriving (Eq, Ord, Bounded, Generic, Show, Read, Typeable)

instance (Finitary a) => Finitary (PackInto p a)

newtype instance VU.MVector s (PackInto p a) = MV_PackInto (VU.MVector s p)

instance (Finitary p, Finitary a, Cardinality a <= Cardinality p, VU.Unbox p) => VGM.MVector VU.MVector (PackInto p a) where
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
  basicUnsafeReplicate len = fmap MV_PackInto . VGM.basicUnsafeReplicate len . mungeOut
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_PackInto v) = fmap mungeInto . VGM.basicUnsafeRead v
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_PackInto v) i = VGM.basicUnsafeWrite v i . mungeOut 
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

newtype instance VU.Vector (PackInto p a) = V_PackInto (VU.Vector p)

instance (Finitary p, Finitary a, Cardinality a <= Cardinality p, VU.Unbox p) => VG.Vector VU.Vector (PackInto p a) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = fmap V_PackInto . VG.basicUnsafeFreeze . op MV_PackInto
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = fmap MV_PackInto . VG.basicUnsafeThaw . op V_PackInto
  {-# INLINE basicLength #-}
  basicLength = over V_PackInto VG.basicLength
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over V_PackInto (VG.basicUnsafeSlice i len)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_PackInto v) = fmap mungeInto . VG.basicUnsafeIndexM v
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_PackInto dst) (V_PackInto src) = VG.basicUnsafeCopy dst src

instance (Finitary p, Finitary a, Cardinality a <= Cardinality p, VU.Unbox p) => VU.Unbox (PackInto p a)

instance (Finitary p, Finitary a, Cardinality a <= Cardinality p, Storable p) => Storable (PackInto p a) where
  {-# INLINE sizeOf #-}
  sizeOf _ = sizeOf (undefined :: p)
  {-# INLINE alignment #-}
  alignment _ = alignment (undefined :: p)
  {-# INLINE peek #-}
  peek = fmap mungeInto . peek @p . castPtr
  {-# INLINE poke #-}
  poke ptr = poke @p (castPtr ptr) . mungeOut 

instance (Finitary p, Finitary a, Cardinality a <= Cardinality p, Binary p, Integral p) => Binary (PackInto p a) where
  put (PackInto x) = do let ix = fromIntegral @_ @p . toFinite $ x
                        let card = fromIntegral @_ @p . subtract 1 . natVal @(Cardinality a) $ Proxy
                        put card >> put ix
  get = do card <- get @p
           ix <- get @p
           if ix <= card
           then pure . fromFinite . fromIntegral $ ix
           else empty

-- Helpers

{-# INLINE mungeInto #-}
mungeInto :: (Finitary p, Finitary a, Cardinality a <= Cardinality p) => p -> PackInto p a
mungeInto = PackInto . fromFinite . fromJust . strengthenN . toFinite

{-# INLINE mungeOut #-}
mungeOut :: (Finitary p, Finitary a, Cardinality a <= Cardinality p) => PackInto p a -> p
mungeOut = fromFinite . weakenN . toFinite . unpackFrom  
