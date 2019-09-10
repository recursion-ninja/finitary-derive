{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Finitary.Finiteness 
(
  Finiteness(..)
) where

import GHC.Generics (Generic)
import Type.Reflection (Typeable)
import GHC.TypeNats
import Data.Ord (comparing)
import CoercibleUtils (over2)
import Data.Finitary (Finitary(..))

newtype Finiteness a = Finiteness a
  deriving (Show, Read, Generic, Typeable)

instance (Finitary a) => Finitary (Finiteness a)

-- If our indexes are equal, so are we
instance (Finitary a) => Eq (Finiteness a) where
  (==) = over2 Finiteness (equating toFinite)

-- Order based on index order
instance (Finitary a) => Ord (Finiteness a) where
  compare = over2 Finiteness (comparing toFinite) 

-- Bounds are the same as the index bounds
instance (Finitary a, 1 <= Cardinality a) => Bounded (Finiteness a) where
  minBound = Finiteness start
  maxBound = Finiteness end

-- Helpers
equating :: (Eq b) => (a -> b) -> a -> a -> Bool
equating p x y = p x == p y
