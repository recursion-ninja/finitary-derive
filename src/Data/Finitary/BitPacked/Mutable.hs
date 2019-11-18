{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Finitary.BitPacked.Mutable 
(
  MVector
) where

import CoercibleUtils (over2)
import GHC.TypeNats
import Data.Kind (Type)
import Data.Finitary (Finitary(..))
import GHC.TypeLits.Extra
import Data.Proxy (Proxy(..))
import Control.Monad.Trans.State.Strict (evalState, get, modify)
import Numeric.Natural (Natural)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Bit.ThreadSafe as BT

-- yurete yuganda sekai ni dan dan boku wa...
newtype MVector (s :: Type) (a :: Type) = MVector { unravel :: VU.MVector s BT.Bit }

instance (Finitary a, 1 <= Cardinality a) => VGM.MVector MVector a where
  {-# INLINE basicLength #-}
  basicLength = (`div` bitLength @a) . VGM.basicLength . unravel
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = MVector . VGM.basicUnsafeSlice (i * bitLength @a) (len * bitLength @a) . unravel
  {-# INLINE basicOverlaps #-}
  basicOverlaps = over2 MVector VGM.basicOverlaps
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew len = MVector <$> VGM.basicUnsafeNew (len * bitLength @a)
  {-# INLINE basicInitialize #-}
  basicInitialize = VGM.basicInitialize . unravel
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVector v) i = fmap (VGM.unsafeCopy sliced) . VG.thaw . intoBits
    where sliced = VGM.unsafeSlice v (i * bitLength @a) (bitLength @a) 

{-# INLINE intoBits #-}
intoBits :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  a -> VU.Vector BT.Bit
intoBits x = let len = bitLength @a in
                evalState (VU.replicateM len go) (fromIntegral @_ @Natural . toFinite $ x)
  where go = do current <- get
                modify (`div` 2)
                return . BT.Bit . odd $ current

{-# INLINE bitLength #-}
bitLength :: forall (a :: Type) (b :: Type) . 
  (Finitary a, 1 <= Cardinality a, Num b) =>
  b
bitLength = fromIntegral . natVal $ (Proxy :: Proxy (CLog 2 (Cardinality a)))
