{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE TypeInType #-}

module Data.Finitary.PackWords where

import Data.Kind (Type)

import qualified Data.Vector.Unboxed as VU

newtype PackWords (a :: Type) = PackWords (VU.Vector Word)
  deriving (Eq, Ord)
