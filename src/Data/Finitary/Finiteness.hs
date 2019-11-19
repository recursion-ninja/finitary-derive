{-
 - Copyright (C) 2019  Koz Ross <koz.ross@retro-freedom.nz>
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module:        Data.Finitary.Finiteness
-- Description:   Newtype wrapper for deriving various typeclasses for
--                @Finitary@ types.
-- Copyright:     (C) Koz Ross 2019
-- License:       GPL version 3.0 or later
-- Maintainer:    koz.ross@retro-freedom.nz
-- Stability:     Experimental
-- Portability:   GHC only
--
-- Knowing that a type @a@ is an instance of @Finitary@ gives us the knowledge
-- that there is an isomorphism between @a@ and @Finite n@ for some @KnownNat
-- n@. This gives us a lot of information, which we can exploit to automagically
-- derive a range of type class instances.
--
-- 'Finiteness' is a @newtype@ wrapper providing this functionality, while
-- 're-exporting' as many type class instances of the underlying type as
-- possible. It is designed for use with @DerivingVia@ - an example of use:
--
-- > {-# LANGUAGE DerivingVia #-}
-- > {-# LANGUAGE DeriveAnyClass #-}
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import GHC.Generics
-- > import Data.Finitary
-- > import Data.Finitary.Finiteness
-- > import Data.Word
-- > import Control.DeepSeq
-- > import Data.Hashable
-- > import Data.Binary
-- > 
-- > data Foo = Bar | Baz (Word8, Word8) | Quux Word16
-- >  deriving (Eq, Generic, Finitary)
-- >  deriving (Ord, Bounded, NFData, Hashable, Binary) via Finiteness
--
-- Currently, the following type class instances can be derived in this manner:
--
-- * 'Ord'
-- * 'Bounded'
-- * 'NFData'
-- * 'Hashable'
-- * 'Binary'
--
-- Additionally, 'Finiteness' \'forwards\' definitions of the following type
-- classes:
--
-- * 'Eq'
-- * 'Show'
-- * 'Read'
-- * 'Typeable'
-- * 'Data'
-- * 'Semigroup'
-- * 'Monoid'
module Data.Finitary.Finiteness 
(
  Finiteness(..)
) where

import GHC.TypeNats
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Finitary (Finitary(..))
import Data.Ord (comparing)
import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..))
import Data.Binary (Binary(..))

-- | Essentially 'Data.Functor.Identity' with a different name. Named this way due to the
-- wordplay you get from use with @DerivingVia@.
newtype Finiteness a = Finiteness { unFiniteness :: a }
  deriving (Eq, Show, Read, Typeable, Data, Functor, Semigroup, Monoid)

-- | 'Finiteness' merely replicates the @Finitary@ behaviour of the underlying
-- type.
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

-- | 'Ord' can be derived by deferring to the order on @Finite (Cardinality a)@.
instance (Finitary a) => Ord (Finiteness a) where
  {-# INLINE compare #-}
  compare (Finiteness x) (Finiteness y) = comparing toFinite x y

-- | Since any inhabited 'Finitary' type is also 'Bounded', we can forward this
-- definition also.
instance (Finitary a, 1 <= Cardinality a) => Bounded (Finiteness a) where
  {-# INLINE minBound #-}
  minBound = Finiteness start
  {-# INLINE maxBound #-}
  maxBound = Finiteness end

-- | We can force evaluation of a 'Finitary' type by converting it to its index.
instance (Finitary a) => NFData (Finiteness a) where
  {-# INLINE rnf #-}
  rnf = rnf . toFinite . unFiniteness

-- | Any 'Finitary' type can be hashed by hashing its index.
instance (Finitary a) => Hashable (Finiteness a) where 
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt = hashWithSalt salt . fromIntegral @_ @Integer . toFinite . unFiniteness

-- | Any 'Finitary' type can be converted to a binary representation by
-- converting its index.
instance (Finitary a) => Binary (Finiteness a) where
  {-# INLINE put #-}
  put = put . fromIntegral @_ @Integer . toFinite . unFiniteness
  {-# INLINE get #-}
  get = Finiteness . fromFinite . fromIntegral @Integer <$> get
