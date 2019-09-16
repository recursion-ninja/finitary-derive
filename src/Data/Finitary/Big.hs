{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Finitary.Big where

import Numeric.Natural (Natural)
import GHC.Generics (Generic, Generic1)
import Type.Reflection (Typeable)
import Data.Finitary (Finitary(..))
import Data.Binary (Binary(..))
import Data.Data (Data)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable(..))
import GHC.TypeNats
import GHC.TypeLits.Extra
import Data.Word (Word64)
import Data.Finite (Finite)
import Data.Proxy (Proxy(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr)
import CoercibleUtils (op, over, over2)
import Data.Foldable (traverse_)

import qualified Control.Monad.State.Strict as MS 
import qualified Data.Vector.Storable.Sized as VSS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic.Mutable as VGM

newtype Big a = Big { reduce :: a }
  deriving (Eq, Ord, Bounded, Generic, Show, Read, Typeable, Data, Generic1, Functor, Semigroup, Monoid, Num) 

instance (NFData a) => NFData (Big a)

instance (Finitary a) => Hashable (Big a) where
  hashWithSalt salt = hashWithSalt salt . fromIntegral @_ @Natural . toFinite . reduce

instance (Finitary a) => Finitary (Big a)

instance (Finitary a) => Binary (Big a) where
  {-# INLINE put #-}
  put = put . fromIntegral @_ @Natural . toFinite . reduce
  {-# INLINE get #-}
  get = Big . fromFinite . fromIntegral <$> get @Natural

instance (Finitary a, 1 <= Cardinality a) => Storable (Big a) where
  {-# INLINE sizeOf #-}
  sizeOf _ = sizeOf (undefined :: VSS.Vector (UnrollVectorLen a) Word64)
  {-# INLINE alignment #-}
  alignment _ = alignment (undefined :: VSS.Vector (UnrollVectorLen a) Word64)
  {-# INLINE peek #-}
  peek = fmap (Big . fromFinite . roll @a) . peek @(VSS.Vector (UnrollVectorLen a) Word64) . castPtr
  {-# INLINE poke #-}
  poke ptr = poke (castPtr ptr) . unroll @a . toFinite . reduce

newtype instance VU.MVector s (Big a) = MV_Big (VU.MVector s Word64) 

-- MVector
instance (Finitary a, 1 <= Cardinality a) => VGM.MVector VU.MVector (Big a) where
  {-# INLINE basicLength #-}
  basicLength = (\x -> x `div` lenOf @a) . VGM.basicLength . op MV_Big
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over MV_Big (VGM.basicUnsafeSlice (i * lenOf @a) (len * lenOf @a))
  {-# INLINE basicOverlaps #-}
  basicOverlaps = over2 MV_Big VGM.basicOverlaps
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew len = MV_Big <$> VGM.basicUnsafeNew (len * lenOf @a)
  {-# INLINE basicInitialize #-}
  basicInitialize = VGM.basicInitialize . op MV_Big
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_Big v) = _
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_Big v) i x = do let arr = unroll . toFinite . reduce $ x
                                       let ixes = [i .. (i * lenOf @a - 1)]
                                       traverse_ (\j -> VGM.basicUnsafeWrite v j _) ixes

-- Vector
-- argh

-- Helpers
type UnrollVectorLen a = CLog (Cardinality Word64) (Cardinality a)

{-# INLINE lenOf #-}
lenOf :: forall a . (Finitary a, 1 <= Cardinality a) => Int
lenOf = fromIntegral . natVal @(UnrollVectorLen a) $ Proxy

{-# INLINE unroll #-}
unroll :: (Finitary a, 1 <= Cardinality a) => Finite (Cardinality a) -> VSS.Vector (UnrollVectorLen a) Word64
unroll = MS.evalState (VSS.replicateM go) . fromIntegral @_ @Natural
  where go = do n <- MS.get
                let (d, r) = quotRem n (natVal @(Cardinality Word64) Proxy)
                MS.put d
                return . fromIntegral $ r

{-# INLINE roll #-}
roll :: (Finitary a) => VSS.Vector (UnrollVectorLen a) Word64 -> Finite (Cardinality a)
roll v = MS.evalState (VSS.foldM go 0 v) 1
  where go acc w = do power <- MS.get
                      MS.modify (\x -> x * natVal @(Cardinality Word64) Proxy)
                      return (acc + fromIntegral power * fromIntegral w)
