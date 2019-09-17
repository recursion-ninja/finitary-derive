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
