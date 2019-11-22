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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Main where

import Data.Kind (Type)
import GHC.TypeNats
import GHC.Generics (Generic)
import Data.Word (Word8, Word16, Word64)
import Hedgehog
import Hedgehog.Classes
import Data.Finitary (Finitary(..))
import Data.Finite (Finite)
import Data.Proxy (Proxy(..))
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable(..))
import Data.Binary (Binary)
import Foreign.Storable (Storable)

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Data.Finitary.Finiteness (Finiteness(..))
import Data.Finitary.PackBytes (PackBytes)
import Data.Finitary.PackWords (PackWords)
import Data.Finitary.PackInto (PackInto)

data Foo = Bar | Baz Word8 Word8 | Quux Word16
  deriving (Eq, Show, Generic, Finitary)
  deriving (Ord, Bounded, NFData, Hashable, Binary) via (Finiteness Foo)

data Big = Big Word64 Word64
  deriving (Eq, Show, Generic, Finitary)
  deriving (Ord, Bounded, NFData, Hashable, Binary) via (Finiteness Big)

-- Generators
choose :: forall (a :: Type) m . (MonadGen m, Finitary a) => m a
choose = fromFinite <$> chooseFinite

chooseFinite :: forall (n :: Nat) m . (KnownNat n, MonadGen m) => m (Finite n)
chooseFinite = fromIntegral <$> G.integral (R.linear 0 limit)
  where limit = subtract @Integer 1 . fromIntegral . natVal @n $ Proxy

finitenessLaws :: (Show a, Binary a, Ord a) => Gen a -> [Laws]
finitenessLaws p = [binaryLaws p, ordLaws p]

packLaws :: (Eq a, Show a, Storable a) => Gen a -> [Laws]
packLaws p = [storableLaws p]

finitenessTests :: [(String, [Laws])]
finitenessTests = [("Small Finiteness", finitenessLaws @Foo choose),
                   ("Big Finiteness", finitenessLaws @Big choose)]

packTests :: [(String, [Laws])]
packTests = [("Small PackBytes", packLaws @(PackBytes Foo) choose),
             ("Big PackBytes", packLaws @(PackBytes Big) choose),
             ("Small PackWords", packLaws @(PackWords Foo) choose),
             ("Big PackWords", packLaws @(PackWords Big) choose),
             ("Small packed into Word64", packLaws @(PackInto Foo Word64) choose)]

main :: IO Bool
main = (&&) <$> lawsCheckMany finitenessTests <*> lawsCheckMany packTests
