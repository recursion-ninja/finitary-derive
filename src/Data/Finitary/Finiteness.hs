{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Data.Finitary.Finiteness 
(
  Finiteness(..)
) where

import GHC.TypeNats
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Finitary (Finitary(..), inhabitantsFromTo)
import Data.Ord (comparing)
import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..))
import Data.Binary (Binary(..))

newtype Finiteness a = Finiteness { unFiniteness :: a }
  deriving (Eq, Show, Read, Typeable, Data, Functor, Semigroup, Monoid)

instance (Finitary a) => Finitary (Finiteness a) where
  type Cardinality (Finiteness a) = Cardinality a
  {-# INLINE fromFinite #-}
  fromFinite = Finiteness . fromFinite
  {-# INLINE toFinite #-}
  toFinite = toFinite . unFiniteness
  {-# INLINE start #-}
  start = Finiteness start
  {-# INLINE end #-}
  end = Finiteness end
  {-# INLINE previous #-}
  previous = fmap Finiteness . previous . unFiniteness
  {-# INLINE next #-}
  next = fmap Finiteness . next . unFiniteness

instance (Finitary a) => Ord (Finiteness a) where
  {-# INLINE compare #-}
  compare (Finiteness x) (Finiteness y) = comparing toFinite x y

instance (Finitary a, 1 <= Cardinality a) => Bounded (Finiteness a) where
  {-# INLINE minBound #-}
  minBound = Finiteness start
  {-# INLINE maxBound #-}
  maxBound = Finiteness end

instance (Finitary a) => NFData (Finiteness a) where
  {-# INLINE rnf #-}
  rnf = rnf . toFinite . unFiniteness

instance (Finitary a) => Hashable (Finiteness a) where 
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt = hashWithSalt salt . fromIntegral @_ @Integer . toFinite . unFiniteness

instance (Finitary a) => Binary (Finiteness a) where
  {-# INLINE put #-}
  put = put . fromIntegral @_ @Integer . toFinite . unFiniteness
  {-# INLINE get #-}
  get = Finiteness . fromFinite . fromIntegral @Integer <$> get
