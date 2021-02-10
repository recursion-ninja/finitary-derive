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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

module Main where

-- base
import Data.Kind (Type)
import Data.Word (Word8, Word16, Word64)
import Foreign.Storable (Storable)
import GHC.Exts (proxy#)
import GHC.Generics (Generic)
import GHC.TypeNats (Nat, KnownNat, natVal')

-- binary
import Data.Binary (Binary)

-- deepseq
import Control.DeepSeq (NFData)

-- finitary
import Data.Finitary (Finitary(..))

-- finitary-derive
import Data.Finitary.Finiteness (Finiteness(..))
import Data.Finitary.PackBytes (PackBytes)
import Data.Finitary.PackWords (PackWords)
import Data.Finitary.PackInto (PackInto)
import qualified Data.Finitary.PackBits as Safe
import qualified Data.Finitary.PackBits.Unsafe as Unsafe
import qualified Data.Finitary.PackBytes as PackBytes
import qualified Data.Finitary.PackWords as PackWords

-- finite-typelits
import Data.Finite (Finite)

-- hashable
import Data.Hashable (Hashable(..))

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

-- hedgehog-classes
import Hedgehog.Classes

-- vector
import Data.Vector.Unboxed (Unbox)

--------------------------------------------------------------------------------

data Foo = Bar | Baz Word8 Word16 | Quux Word16
  deriving (Eq, Show, Generic, Finitary)
  deriving (Ord, Bounded, NFData, Hashable, Binary) via (Finiteness Foo)

data Big = Big Word64 Word64 | Mediums Foo Foo Foo Foo
  deriving (Eq, Show, Generic, Finitary)
  deriving (Ord, Bounded, NFData, Hashable, Binary) via (Finiteness Big)

genFoo :: MonadGen m => m Foo
genFoo = do
  c <- G.element @_ @Word [ 0, 1, 2 ]
  case c of
    0 -> pure Bar
    1 -> do
      w1 <- G.word8  (R.linear 0 maxBound)
      w2 <- G.word16 (R.linear 0 maxBound)
      pure ( Baz w1 w2 )
    _ -> do
      w1 <- G.word16 (R.linear 0 maxBound)
      pure ( Quux w1 )

genBig :: MonadGen m => m Big
genBig = do
  c <- G.element @_ @Word [ 0, 1 ]
  case c of
    0 -> do
      w1 <- G.word64 (R.linear 0 maxBound)
      w2 <- G.word64 (R.linear 0 maxBound)
      pure ( Big w1 w2 )
    _ -> do
      foo1 <- genFoo
      foo2 <- genFoo
      foo3 <- genFoo
      foo4 <- genFoo
      pure ( Mediums foo1 foo2 foo3 foo4 )

-- Generators
choose :: forall (a :: Type) m . (MonadGen m, Finitary a) => m a
choose = fromFinite <$> chooseFinite

chooseFinite :: forall (n :: Nat) m . (KnownNat n, MonadGen m) => m (Finite n)
chooseFinite = fromIntegral <$> G.integral (R.linear 0 limit)
  where limit = subtract @Integer 1 . fromIntegral $ natVal' @n proxy#

finitenessLaws :: (Show a, Binary a, Ord a) => Gen a -> [Laws]
finitenessLaws p = [binaryLaws p, ordLaws p]

packLaws :: (Eq a, Show a, Storable a) => Gen a -> [Laws]
packLaws p = [storableLaws p]

vectorLaws :: (Eq a, Show a, Unbox a) => Gen a -> [Laws]
vectorLaws p = [muvectorLaws p]

ordIsMonotonic :: forall (a :: Type) (t :: Type -> Type) . 
  (Finitary a, Show a, Ord a, Ord (t a)) => 
  (a -> t a) -> Property
ordIsMonotonic f = property $ do x <- forAll $ choose @a
                                 y <- forAll $ choose @a
                                 (x < y) === (f x < f y)

roundTrips :: forall (a :: Type) (t :: Type -> Type) . 
  (Finitary a, Show a, Ord a) => 
  Gen a -> (a -> t a) -> (t a -> a) -> Property
roundTrips gen pack unpack = property $ do
  a <- forAll $ gen
  case pack a of
    !packed -> case unpack packed of
      roundTripped -> a === roundTripped

finitenessTests :: [(String,[Laws])]
finitenessTests =
  [ ("Small Finiteness", finitenessLaws @Foo choose)
  , ("Big Finiteness"  , finitenessLaws @Big choose)
  ]

packTests :: [(String,[Laws])]
packTests =
  [ ("Small PackBytes"         , packLaws @(PackBytes Foo)       choose)
  , ("Big PackBytes"           , packLaws @(PackBytes Big)       choose)
  , ("Small PackWords"         , packLaws @(PackWords Foo)       choose)
  , ("Big PackWords"           , packLaws @(PackWords Big)       choose)
  , ("Small packed into Word64", packLaws @(PackInto Foo Word64) choose)
  ]

vectorTests :: [(String,[Laws])]
vectorTests =
  [ ("Small PackBits"       , vectorLaws @(Safe.PackBits   Foo) choose)
  , ("Small unsafe PackBits", vectorLaws @(Unsafe.PackBits Foo) choose)
  , ("Small PackBytes"      , vectorLaws @(PackBytes       Foo) choose)
  , ("Small PackWords"      , vectorLaws @(PackWords       Foo) choose)
  , ("Big PackBits"         , vectorLaws @(Safe.PackBits   Big) choose)
  , ("Big unsafe PackBits"  , vectorLaws @(Unsafe.PackBits Big) choose)
  , ("Big PackBytes"        , vectorLaws @(PackBytes       Big) choose)
  , ("Big PackWords"        , vectorLaws @(PackWords       Big) choose)
  ]

monotonicTests :: Group
monotonicTests = Group "Monotonicity" 
  [ ("Small PackBits"       , ordIsMonotonic @Foo      Safe.Packed)
  , ("Small unsafe PackBits", ordIsMonotonic @Foo    Unsafe.Packed)
  , ("Small PackBytes"      , ordIsMonotonic @Foo PackBytes.Packed)
  , ("Small PackWords"      , ordIsMonotonic @Foo PackWords.Packed)
  , ("Big PackBits"         , ordIsMonotonic @Big      Safe.Packed)
  , ("Big unsafe PackBits"  , ordIsMonotonic @Big    Unsafe.Packed)
  , ("Big PackBytes"        , ordIsMonotonic @Big PackBytes.Packed)
  , ("Big PackWords"        , ordIsMonotonic @Big PackWords.Packed)
  ]

roundTripTests :: Group
roundTripTests = Group "Round-tripping" 
  [ ("Small PackBits"       , roundTrips @Foo genFoo      Safe.Packed ( \ (      Safe.Packed x ) -> x ) )
  , ("Small unsafe PackBits", roundTrips @Foo genFoo    Unsafe.Packed ( \ (    Unsafe.Packed x ) -> x ) )
  , ("Small PackBytes"      , roundTrips @Foo genFoo PackBytes.Packed ( \ ( PackBytes.Packed x ) -> x ) )
  , ("Small PackWords"      , roundTrips @Foo genFoo PackWords.Packed ( \ ( PackWords.Packed x ) -> x ) )
  , ("Big PackBits"         , roundTrips @Big genBig      Safe.Packed ( \ (      Safe.Packed x ) -> x ) )
  , ("Big unsafe PackBits"  , roundTrips @Big genBig    Unsafe.Packed ( \ (    Unsafe.Packed x ) -> x ) )
  , ("Big PackBytes"        , roundTrips @Big genBig PackBytes.Packed ( \ ( PackBytes.Packed x ) -> x ) )
  , ("Big PackWords"        , roundTrips @Big genBig PackWords.Packed ( \ ( PackWords.Packed x ) -> x ) )
  ]

checkTest :: Either [(String,[Laws])] Group -> IO Bool
checkTest ( Left  laws  ) = lawsCheckMany laws
checkTest ( Right group ) = checkParallel group

main :: IO Bool
main = and <$> traverse checkTest [ Left finitenessTests, Left packTests, Left vectorTests, Right monotonicTests, Right roundTripTests ]
