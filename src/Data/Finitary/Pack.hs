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

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | 
-- Module:        Data.Finitary.Pack
-- Description:   A wrapper around @Finitary@ types, designed to provide easy
--                derivation of @Storable@, @Binary@ and @Unbox@ instances.
-- Copyright:     (C) Koz Ross, 2019
-- License:       GPL version 3.0 or later
-- Maintainer:    koz.ross@retro-freedom.nz
-- Stability:     Experimental
-- Portability:   GHC only
--
-- Defines a newtype for easy derivation of 'Data.Vector.Unboxed.Unbox', 'Storable', 
-- 'Data.Binary.Binary' and 'Hashable' instances for any type with a 'Finitary' instance. The easiest way to use
-- this is with the @DerivingVia@ extension:
--
-- > {-# LANGUAGE DeriveAnyClass #-}
-- > {-# LANGUAGE DeriveGeneric #-}
-- > {-# LANGUAGE DerivingVia #-}
-- >
-- > import Data.Finitary
-- > import Data.Finitary.Pack
-- > import Data.Word
-- > import Data.Hashable
-- >
-- > data Foo = Bar | Baz (Word8, Word8) | Quux Word16
-- >  deriving (Eq, Generic, Finitary)
-- >  deriving (Storable, Binary, Hashable) via (Pack Foo)
-- 
-- Alternatively, you can just use @Pack a@ instead of @a@ wherever appropriate.
-- Unfortunately (due to role restrictions on unboxed vectors), you /must/ use
-- @Pack a@ if you want a 'Data.Vector.Unboxed.Vector' full of @a@s -
-- @DerivingVia@ is of no help here.
module Data.Finitary.Pack 
(
  Pack(..)
) where

import Data.Foldable (traverse_)
import CoercibleUtils (op, over, over2)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic, Generic1)
import Data.Data (Data)
import Type.Reflection (Typeable)
import Data.Finitary (Finitary(..))
import GHC.TypeNats
import GHC.TypeLits.Extra
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Control.Monad.State.Strict (evalState, MonadState(..), modify)
import Data.Proxy (Proxy(..))
import Data.Hashable (Hashable(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr)

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Generic.Sized as VGS
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Sized as VSS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Sized as VUS
import qualified Data.Binary as B

-- | Essentially @Identity a@, but with different instances. So named due to the \'packing\' of the 
-- type's indices densely into arrays, memory or bits respectively.
newtype Pack a = Pack { unPack :: a }
  deriving (Eq, Ord, Bounded, Generic, Show, Read, Typeable, Data, Generic1, Functor, Semigroup, Monoid)

instance (NFData a) => NFData (Pack a)

instance (Finitary a) => Finitary (Pack a)

-- | We can hash any @Finitary@ by hashing its index.
instance (Finitary a, 1 <= Cardinality a) => Hashable (Pack a) where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt = hashWithSalt salt . packWords @VU.Vector 

-- | We can serialize any @Finitary@ by serializing its index.
instance (Finitary a, 1 <= Cardinality a) => B.Binary (Pack a) where
  {-# INLINE put #-}
  put = B.put . packWords @VU.Vector
  {-# INLINE get #-}
  get = unpackWords @VU.Vector <$> B.get

-- | As @Finitary@ instances have known limits on their indices, they can be
-- stored as their indices.
instance (Finitary a, 1 <= Cardinality a) => Storable (Pack a) where
  {-# INLINE sizeOf #-}
  sizeOf _ = sizeOf (undefined :: VSS.Vector (WordCount a) Word8)
  {-# INLINE alignment #-}
  alignment _ = alignment (undefined :: VSS.Vector (WordCount a) Word8)
  {-# INLINE peek #-}
  peek = fmap unpackWords . peek @(VSS.Vector (WordCount a) Word8) . castPtr
  {-# INLINE poke #-}
  poke ptr = poke (castPtr ptr) . packWords @VS.Vector

newtype instance VU.MVector s (Pack a) = MV_Pack (VU.MVector s Word8)

instance (Finitary a, 1 <= Cardinality a) => VGM.MVector VU.MVector (Pack a) where
  {-# INLINE basicLength #-}
  basicLength = (\x -> x `div` lenOf @a) . VGM.basicLength . op MV_Pack
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over MV_Pack (VGM.basicUnsafeSlice (i * lenOf @a) (len * lenOf @a))
  {-# INLINE basicOverlaps #-}
  basicOverlaps = over2 MV_Pack VGM.basicOverlaps
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew len = MV_Pack <$> VGM.basicUnsafeNew (len * lenOf @a)
  {-# INLINE basicInitialize #-}
  basicInitialize = VGM.basicInitialize . op MV_Pack
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_Pack v) i = unpackWords <$> VSS.generateM (VGM.basicUnsafeRead v . (i +) . fromIntegral)
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_Pack v) i x = do let arr = packWords x
                                        let ixes = [i .. (i * lenOf @a - 1)]
                                        traverse_ (\j -> VGM.basicUnsafeWrite v j (VUS.unsafeIndex arr (j - i))) ixes

newtype instance VU.Vector (Pack a) = V_Pack (VU.Vector Word8)

instance (Finitary a, 1 <= Cardinality a) => VG.Vector VU.Vector (Pack a) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = fmap V_Pack . VG.basicUnsafeFreeze . op MV_Pack
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = fmap MV_Pack . VG.basicUnsafeThaw . op V_Pack
  {-# INLINE basicLength #-}
  basicLength = over V_Pack VG.basicLength
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over V_Pack (VG.basicUnsafeSlice (i * lenOf @a) (len * lenOf @a))
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Pack v) i = unpackWords <$> VSS.generateM (VG.basicUnsafeIndexM v . (i +) . fromIntegral)

-- | We can rely on the fact that indexes of any @Finitary@ type have a fixed
-- maximum size to \'unravel\' them into a block of 'Word8's, which we can
-- easily unbox.
instance (Finitary a, 1 <= Cardinality a) => VU.Unbox (Pack a)

-- helpers

type WordCount a = CLog (Cardinality Word8) (Cardinality a)

{-# INLINE packWords #-}
packWords :: forall v a . (Finitary a, 1 <= Cardinality a, VG.Vector v Word8) => Pack a -> VGS.Vector v (WordCount a) Word8
packWords = evalState (VGS.replicateM go) . fromIntegral @_ @Natural . toFinite . unPack
  where go = do n <- get
                let (d, r) = quotRem n (natVal @(Cardinality Word8) Proxy)
                put d
                return . fromIntegral $ r

{-# INLINE unpackWords #-}
unpackWords :: forall v a . (Finitary a, VG.Vector v Word8) => VGS.Vector v (WordCount a) Word8 -> Pack a
unpackWords v = Pack . fromFinite . evalState (VGS.foldM go 0 v) $ 1
  where go acc w = do power <- get
                      modify (\x -> x * natVal @(Cardinality Word8) Proxy)
                      return (acc + fromIntegral power * fromIntegral w)

{-# INLINE lenOf #-}  
lenOf :: forall a . (Finitary a, 1 <= Cardinality a) => Int
lenOf = fromIntegral . natVal @(WordCount a) $ Proxy
