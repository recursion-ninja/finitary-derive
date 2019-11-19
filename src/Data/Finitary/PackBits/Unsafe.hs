{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Finitary.PackBits.Unsafe 
(
  PackBits, pattern Packed
) where

import GHC.TypeLits.Extra
import Data.Proxy (Proxy(..))
import Numeric.Natural (Natural)
import GHC.TypeNats
import CoercibleUtils (op, over, over2)
import Data.Kind (Type)
import Data.Hashable (Hashable(..))
import Data.Vector.Instances ()
import Control.DeepSeq (NFData(..))
import Data.Finitary(Finitary(..))
import Data.Finite (Finite)
import Control.Monad.Trans.State.Strict (evalState, get, modify)

import qualified Data.Bit as B
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

newtype PackBits (a :: Type) = PackBits (VU.Vector B.Bit)
  deriving (Eq, Ord)

type role PackBits nominal

pattern Packed :: forall (a :: Type) . 
  (Finitary a) => 
  PackBits a -> a
pattern Packed x <- (packBits -> x)
  where Packed x = unpackBits x

instance Hashable (PackBits a) where
  hashWithSalt salt = hashWithSalt salt . B.cloneToWords . op PackBits

instance NFData (PackBits a) where
  rnf = rnf . op PackBits

instance (Finitary a) => Finitary (PackBits a) where
  type Cardinality (PackBits a) = Cardinality a
  fromFinite = PackBits . intoBits
  toFinite = outOfBits . op PackBits

instance (Finitary a, 1 <= Cardinality a) => Bounded (PackBits a) where
  minBound = start
  maxBound = end

newtype instance VU.MVector s (PackBits a) = MV_PackBits (VU.MVector s B.Bit)

instance (Finitary a, 1 <= Cardinality a) => VGM.MVector VU.MVector (PackBits a) where
  basicLength = over MV_PackBits ((`div` bitLength @a) . VGM.basicLength)
  basicOverlaps = over2 MV_PackBits VGM.basicOverlaps
  basicUnsafeSlice i len = over MV_PackBits (VGM.basicUnsafeSlice (i * bitLength @a) (len * bitLength @a))
  basicUnsafeNew len = fmap MV_PackBits (VGM.basicUnsafeNew (len * bitLength @a))
  basicInitialize = VGM.basicInitialize . op MV_PackBits
  basicUnsafeRead (MV_PackBits v) i = fmap PackBits . VG.freeze . VGM.unsafeSlice (i * bitLength @a) (bitLength @a) $ v
  basicUnsafeWrite (MV_PackBits v) i (PackBits x) = let slice = VGM.unsafeSlice (i * bitLength @a) (bitLength @a) v in
                                                      VG.unsafeCopy slice x

newtype instance VU.Vector (PackBits a) = V_PackBits (VU.Vector B.Bit)

instance (Finitary a, 1 <= Cardinality a) => VG.Vector VU.Vector (PackBits a) where
  basicLength = over V_PackBits ((`div` bitLength @a) . VG.basicLength)
  basicUnsafeFreeze = fmap V_PackBits . VG.basicUnsafeFreeze . op MV_PackBits
  basicUnsafeThaw = fmap MV_PackBits . VG.basicUnsafeThaw . op V_PackBits
  basicUnsafeSlice i len = over V_PackBits (VG.basicUnsafeSlice (i * bitLength @a) (len * bitLength @a))
  basicUnsafeIndexM (V_PackBits v) i = pure . PackBits . VG.unsafeSlice (i * bitLength @a) (bitLength @a) $ v

instance (Finitary a, 1 <= Cardinality a) => VU.Unbox (PackBits a)

-- Helpers

type BitLength a = CLog 2 (Cardinality a)

packBits :: forall (a :: Type) . 
  (Finitary a) => 
  a -> PackBits a
packBits = fromFinite . toFinite

unpackBits :: forall (a :: Type) . 
  (Finitary a) => 
  PackBits a -> a
unpackBits = fromFinite . toFinite

bitLength :: forall (a :: Type) (b :: Type) . 
  (Finitary a, 1 <= Cardinality a, Num b) => 
  b
bitLength = fromIntegral . natVal $ (Proxy :: Proxy (BitLength a))

intoBits :: forall (n :: Nat) .
  (KnownNat n) =>  
  Finite n -> VU.Vector B.Bit
intoBits = VU.unfoldr go . fromIntegral @_ @Natural
  where go 0 = Nothing
        go n = let (d, r) = quotRem n 2 in
                 Just (B.Bit . toEnum . fromIntegral $ r, d)

outOfBits :: forall (n :: Nat) .
  (KnownNat n) =>  
  VU.Vector B.Bit -> Finite n
outOfBits v = evalState (VU.foldM' go 0 v) 1
  where go old (B.Bit b) = do power <- get
                              let placeValue = power * (fromIntegral . fromEnum $ b)
                              modify (* 2)
                              return (old + placeValue)