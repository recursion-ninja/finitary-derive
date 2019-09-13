{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Finitary.Big where

import Control.Applicative (Alternative(..))
import Data.Bool (bool)
import Numeric.Natural (Natural)
import GHC.Generics (Generic, Generic1)
import Type.Reflection (Typeable)
import Data.Finitary (Finitary(..))
import Data.Binary (Binary(..))
import GHC.TypeNats
import Data.Proxy (Proxy(..))
import Data.Data (Data)
import Control.DeepSeq (NFData)

newtype Big a = Big { reduce :: a }
  deriving (Eq, Ord, Bounded, Generic, Show, Read, Typeable, Data, Generic1, Functor) 

instance (NFData a) => NFData (Big a)

instance (Finitary a) => Finitary (Big a)

instance (Finitary a) => Binary (Big a) where
  {-# INLINE get #-}
  get = do card <- get @Natural
           ix <- get @Natural
           decodeWith card ix
  {-# INLINE put #-}
  put (Big x) = do let ix = fromIntegral @_ @Natural . toFinite $ x
                   let card = fromIntegral @_ @Natural . subtract 1 . natVal @(Cardinality a) $ Proxy
                   put card >> put ix

-- Helpers

{-# INLINE decodeWith #-}
decodeWith :: (Alternative f, Integral n, Finitary a) => n -> n -> f (Big a)
decodeWith card ix = bool empty (pure . Big . fromFinite . fromIntegral $ ix) (ix <= card)
