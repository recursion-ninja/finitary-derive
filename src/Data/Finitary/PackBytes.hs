{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Data.Finitary.PackBytes 
(
  PackBytes, pattern Packed
) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits.Extra
import GHC.TypeNats
import CoercibleUtils (op, over)
import Data.Kind (Type)
import Data.Word (Word8)
import Data.Vector.Instances ()
import Data.Hashable (Hashable(..))
import Control.DeepSeq (NFData(..))
import Data.Finitary (Finitary(..))
import Numeric.Natural (Natural)
import Data.Finite (Finite)
import Control.Monad.Trans.State.Strict (evalState, get, modify)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic.Mutable as VGM

newtype PackBytes (a :: Type) = PackBytes (VU.Vector Word8)
  deriving (Eq, Ord)

type role PackBytes nominal

pattern Packed :: forall (a :: Type) . 
  (Finitary a) => 
  PackBytes a -> a
pattern Packed x <- (packBytes -> x)
  where Packed x = unpackBytes x

instance Hashable (PackBytes a) where
  hashWithSalt salt = hashWithSalt salt . op PackBytes

instance NFData (PackBytes a) where
  rnf = rnf . op PackBytes

instance (Finitary a) => Finitary (PackBytes a) where
  type Cardinality (PackBytes a) = Cardinality a
  fromFinite = PackBytes . intoBytes
  toFinite = outOfBytes . op PackBytes

newtype instance VU.MVector s (PackBytes a) = MV_PackBytes (VU.MVector s Word8)

instance (Finitary a) => VGM.MVector VU.MVector (PackBytes a) where
  basicLength = over MV_PackBytes ((`div` byteLength @a) . VGM.basicLength)

-- Helpers

type ByteLength a = CLog (Cardinality Word8) (Cardinality a)

byteLength :: forall (a :: Type) (b :: Type) . 
  (Finitary a, 1 <= Cardinality a, Num b) =>
  b
byteLength = fromIntegral . natVal $ (Proxy :: Proxy (ByteLength a)) 

packBytes :: forall (a :: Type) . 
  (Finitary a) => 
  a -> PackBytes a
packBytes = fromFinite . toFinite

unpackBytes :: forall (a :: Type) . 
  (Finitary a) => 
  PackBytes a -> a
unpackBytes = fromFinite . toFinite

intoBytes :: forall (n :: Nat) . 
  (KnownNat n) => 
  Finite n -> VU.Vector Word8
intoBytes = VU.unfoldr go . fromIntegral @_ @Natural
  where go 0 = Nothing
        go n = let (d, r) = quotRem n 256 in
                  Just (fromIntegral r, d)

outOfBytes :: forall (n :: Nat) . 
  (KnownNat n) =>
  VU.Vector Word8 -> Finite n
outOfBytes v = evalState (VU.foldM' go 0 v) 1
  where go old w = do power <- get
                      let placeValue = power * fromIntegral w
                      modify (* 256)
                      return (old + placeValue) 
