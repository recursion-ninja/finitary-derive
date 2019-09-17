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
import Control.Monad.Loops (andM)

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Data.Finitary.Pack (Pack)

data Foo = Bar | Baz Word8 Word8 | Quux Word16
  deriving (Eq, Show, Generic, Finitary)

data Big = Big Word64 Word64
  deriving (Eq, Show, Generic, Finitary)

-- Generators
choose :: forall (a :: Type) m . (MonadGen m, Finitary a) => m a
choose = fromFinite <$> chooseFinite

chooseFinite :: forall (n :: Nat) m . (KnownNat n, MonadGen m) => m (Finite n)
chooseFinite = fromIntegral <$> G.integral (R.linear 0 limit)
  where limit = subtract @Integer 1 . fromIntegral . natVal @n $ Proxy

main :: IO Bool
main = andM . fmap lawsCheck $ [binaryLaws @(Pack Foo) choose,
                                binaryLaws @(Pack Big) choose,
                                binaryLaws @(Pack Int) choose,
                                storableLaws @(Pack Foo) choose,
                                storableLaws @(Pack Big) choose,
                                storableLaws @(Pack Int) choose]
