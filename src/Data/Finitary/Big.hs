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

import qualified Control.Monad.State.Strict as MS 
import qualified Data.Vector.Storable.Sized as VSS

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

-- Helpers
type UnrollVectorLen a = CLog (Cardinality Word64) (Cardinality a)

unroll :: (Finitary a, 1 <= Cardinality a) => Finite (Cardinality a) -> VSS.Vector (UnrollVectorLen a) Word64
unroll = MS.evalState (VSS.replicateM go) . fromIntegral @_ @Natural
  where go :: (MS.MonadState Natural m) => m Word64
        go = do n <- MS.get
                _
