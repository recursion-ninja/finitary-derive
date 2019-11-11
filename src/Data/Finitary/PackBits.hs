{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}

module Data.Finitary.PackBits 
(
  PackBits, intoBits, outOfBits
) where

import Data.Kind (Type)
import GHC.TypeLits.Extra
import GHC.TypeNats
import Data.Finitary (Finitary(..))
import Numeric.Natural (Natural)
import Control.Monad.Trans.State.Strict (evalState, get, put, modify)
import Data.Proxy (Proxy(..))
import Data.Hashable (Hashable(..))
import Data.Vector.Instances ()

import qualified Data.Bit.ThreadSafe as BT
import qualified Data.Vector.Unboxed.Sized as VUS 

type BitCount a = CLog 2 (Cardinality a)

newtype PackBits a = PackBits (VUS.Vector (BitCount a) BT.Bit)
  deriving (Eq, Ord)

instance Hashable (PackBits a) where
  hashWithSalt salt (PackBits v) = hashWithSalt salt . BT.cloneToWords . VUS.fromSized $ v

-- Finitary is a must
-- Others?
-- Can't be Storable because alignment

intoBits :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  a -> PackBits a 
intoBits = PackBits . evalState (VUS.replicateM go) . fromIntegral @_ @Natural . toFinite
  where go = do n <- get
                let (d, r) = quotRem n (natVal @2 Proxy)
                put d
                return . BT.Bit . toEnum . fromIntegral $ r

outOfBits :: forall (a :: Type) . 
  (Finitary a) => 
  PackBits a -> a
outOfBits (PackBits v) = fromFinite . evalState (VUS.foldM go 0 v) $ (1 :: Integer)
  where go acc w = do power <- get
                      modify (* 2)
                      return (acc + fromIntegral power * fromIntegral w)
