{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Finitary.SmallAs 
(
  SmallAs(..)
) where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Maybe (fromJust)
import CoercibleUtils (op, over, over2)
import Data.Kind (Type)
import GHC.Generics (Generic, Generic1)
import Data.Finitary (Finitary(..))
import GHC.TypeNats
import Data.Finite (weakenN, strengthenN)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr)
import Type.Reflection (Typeable)
import Data.Binary (Binary(..))
import Data.Data (Data)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable(..))

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

-- p is the 'pack into' type
newtype SmallAs (p :: Type) (a :: Type) = SmallAs { unpackFrom :: a }
  deriving (Eq, Ord, Bounded, Generic, Show, Read, Typeable, Data, Generic1, Functor, Semigroup, Monoid, Num)

instance (NFData a) => NFData (SmallAs p a)

instance (Finitary a) => Finitary (SmallAs p a)

instance (Finitary a, Finitary p, Cardinality a <= Cardinality p, Hashable p) => Hashable (SmallAs p a) where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt = hashWithSalt salt . mungeOut 

newtype instance VU.MVector s (SmallAs p a) = MV_SmallAs (VU.MVector s p)

instance (Finitary p, Finitary a, Cardinality a <= Cardinality p, VU.Unbox p) => VGM.MVector VU.MVector (SmallAs p a) where
  {-# INLINE basicLength #-}
  basicLength = VGM.basicLength . op MV_SmallAs
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over MV_SmallAs (VGM.basicUnsafeSlice i len)
  {-# INLINE basicOverlaps #-}
  basicOverlaps = over2 MV_SmallAs VGM.basicOverlaps
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew = fmap MV_SmallAs . VGM.basicUnsafeNew
  {-# INLINE basicInitialize #-}
  basicInitialize = VGM.basicInitialize . op MV_SmallAs
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate len = fmap MV_SmallAs . VGM.basicUnsafeReplicate len . mungeOut
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_SmallAs v) = fmap mungeInto . VGM.basicUnsafeRead v
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_SmallAs v) i = VGM.basicUnsafeWrite v i . mungeOut 
  {-# INLINE basicClear #-}
  basicClear = VGM.basicClear . op MV_SmallAs
  {-# INLINE basicSet #-}
  basicSet (MV_SmallAs v) = VGM.basicSet v . fromFinite . weakenN . toFinite . unpackFrom
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_SmallAs dst) (MV_SmallAs src) = VGM.basicUnsafeCopy dst src
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MV_SmallAs dst) (MV_SmallAs src) = VGM.basicUnsafeMove dst src
  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow (MV_SmallAs v) = fmap MV_SmallAs . VGM.basicUnsafeGrow v

newtype instance VU.Vector (SmallAs p a) = V_SmallAs (VU.Vector p)

instance (Finitary p, Finitary a, Cardinality a <= Cardinality p, VU.Unbox p) => VG.Vector VU.Vector (SmallAs p a) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = fmap V_SmallAs . VG.basicUnsafeFreeze . op MV_SmallAs
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = fmap MV_SmallAs . VG.basicUnsafeThaw . op V_SmallAs
  {-# INLINE basicLength #-}
  basicLength = over V_SmallAs VG.basicLength
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over V_SmallAs (VG.basicUnsafeSlice i len)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_SmallAs v) = fmap mungeInto . VG.basicUnsafeIndexM v
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_SmallAs dst) (V_SmallAs src) = VG.basicUnsafeCopy dst src

instance (Finitary p, Finitary a, Cardinality a <= Cardinality p, VU.Unbox p) => VU.Unbox (SmallAs p a)

instance (Finitary p, Finitary a, Cardinality a <= Cardinality p, Storable p) => Storable (SmallAs p a) where
  {-# INLINE sizeOf #-}
  sizeOf _ = sizeOf (undefined :: p)
  {-# INLINE alignment #-}
  alignment _ = alignment (undefined :: p)
  {-# INLINE peek #-}
  peek = fmap mungeInto . peek @p . castPtr
  {-# INLINE poke #-}
  poke ptr = poke @p (castPtr ptr) . mungeOut 

instance (Finitary a, Cardinality a <= Cardinality Word8) => Binary (SmallAs Word8 a) where
  {-# INLINE put #-}
  put = put . fromIntegral @_ @Word8 . toFinite . unpackFrom
  {-# INLINE get #-}
  get = SmallAs . fromFinite . fromIntegral <$> get @Word8

instance (Finitary a, Cardinality a <= Cardinality Word16) => Binary (SmallAs Word16 a) where
  {-# INLINE put #-}
  put = put . fromIntegral @_ @Word16 . toFinite . unpackFrom
  {-# INLINE get #-}
  get = SmallAs . fromFinite . fromIntegral <$> get @Word16

instance (Finitary a, Cardinality a <= Cardinality Word32) => Binary (SmallAs Word32 a) where
  {-# INLINE put #-}
  put = put . fromIntegral @_ @Word32 . toFinite . unpackFrom
  {-# INLINE get #-}
  get = SmallAs . fromFinite . fromIntegral <$> get @Word32

instance (Finitary a, Cardinality a <= Cardinality Word64) => Binary (SmallAs Word64 a) where
  {-# INLINE put #-}
  put = put . fromIntegral @_ @Word64 . toFinite . unpackFrom
  {-# INLINE get #-}
  get = SmallAs . fromFinite . fromIntegral <$> get @Word64

-- Helpers

{-# INLINE mungeInto #-}
mungeInto :: (Finitary p, Finitary a, Cardinality a <= Cardinality p) => p -> SmallAs p a
mungeInto = SmallAs . fromFinite . fromJust . strengthenN . toFinite

{-# INLINE mungeOut #-}
mungeOut :: (Finitary p, Finitary a, Cardinality a <= Cardinality p) => SmallAs p a -> p
mungeOut = fromFinite . weakenN . toFinite . unpackFrom  
