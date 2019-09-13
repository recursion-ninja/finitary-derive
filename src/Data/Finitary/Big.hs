{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Finitary.Big where

import Numeric.Natural (Natural)
import GHC.Generics (Generic, Generic1)
import Type.Reflection (Typeable)
import Data.Finitary (Finitary(..))
import Data.Binary (Binary(..))
import Data.Data (Data)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable(..))

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

