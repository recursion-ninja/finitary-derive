{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

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

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

newtype PackInto (a :: Type) (b :: Type) = PackInto b
  deriving (Eq, Ord)

type role PackInto nominal nominal

pattern Packed :: forall (b :: Type) (a :: Type) . 
  (Finitary a, Finitary b, Cardinality a <= Cardinality b) =>
  PackInto a b -> a
pattern Packed x <- (packInto -> x)
  where Packed x = unpackOutOf x

instance (Hashable b) => Hashable (PackInto a b) where
  hashWithSalt salt = over PackInto (hashWithSalt salt)

instance (NFData b) => NFData (PackInto a b) where
  rnf = over PackInto rnf

instance (Storable b) => Storable (PackInto a b) where
  sizeOf = over PackInto sizeOf
  alignment = over PackInto alignment
  peek = fmap PackInto . peek . castPtr
  poke ptr = poke (castPtr ptr) . op PackInto

-- We can pack a into b if the cardinality of b is at least as large as a (could
-- be larger)
instance (Finitary a, Finitary b, Cardinality a <= Cardinality b) => Finitary (PackInto a b) where
  type Cardinality (PackInto a b) = Cardinality a
  fromFinite = PackInto . fromFinite . weakenN
  toFinite = fromJust . strengthenN . toFinite . op PackInto

instance (Finitary a, Finitary b, 1 <= Cardinality a, Cardinality a <= Cardinality b) => Bounded (PackInto a b) where
  minBound = start
  maxBound = end 

newtype instance VU.MVector s (PackInto a b) = MV_PackInto (VU.MVector s b)

instance (VU.Unbox b) => VGM.MVector VU.MVector (PackInto a b) where
  basicLength = over MV_PackInto VGM.basicLength
  basicOverlaps = over2 MV_PackInto VGM.basicOverlaps
  basicUnsafeSlice i len = over MV_PackInto (VGM.basicUnsafeSlice i len)
  basicUnsafeNew len = MV_PackInto <$> VGM.basicUnsafeNew len
  basicInitialize = VGM.basicInitialize . op MV_PackInto
  basicUnsafeRead (MV_PackInto v) i = PackInto <$> VGM.basicUnsafeRead v i
  basicUnsafeWrite (MV_PackInto v) i (PackInto x) = VGM.basicUnsafeWrite v i x

newtype instance VU.Vector (PackInto a b) = V_PackInto (VU.Vector b)

instance (VU.Unbox b) => VG.Vector VU.Vector (PackInto a b) where
  basicLength = over V_PackInto VG.basicLength
  basicUnsafeFreeze = fmap V_PackInto . VG.basicUnsafeFreeze . op MV_PackInto
  basicUnsafeThaw = fmap MV_PackInto . VG.basicUnsafeThaw . op V_PackInto
  basicUnsafeSlice i len = over V_PackInto (VG.basicUnsafeSlice i len)
  basicUnsafeIndexM (V_PackInto v) i = PackInto <$> VG.basicUnsafeIndexM v i

instance (VU.Unbox b) => VU.Unbox (PackInto a b)

-- Helpers

packInto :: forall (b :: Type) (a :: Type) .
  (Finitary a, Finitary b, Cardinality a <= Cardinality b) =>  
  a -> PackInto a b
packInto = fromFinite . toFinite

unpackOutOf :: forall (b :: Type) (a :: Type) . 
  (Finitary a, Finitary b, Cardinality a <= Cardinality b) => 
  PackInto a b -> a
unpackOutOf = fromFinite . toFinite
