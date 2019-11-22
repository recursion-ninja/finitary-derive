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

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module:        Data.Finitary.PackBytes
-- Description:   Scheme for packing @Finitary@ types into @Word@ arrays.
-- Copyright:     (C) Koz Ross 2019
-- License:       GPL version 3.0 or later
-- Maintainer:    koz.ross@retro-freedom.nz
-- Stability:     Experimental
-- Portability:   GHC only
--
-- If a type @a@ is 'Finitary', each inhabitant of @a@ has an index, which can
-- be represented as an unsigned integer, spread across one or more machine
-- words. This unsigned integer will have fixed length (as the number of
-- inhabitants of @a@ is finite). We can use this to derive a 'VU.Unbox'
-- instance, by representing 'VU.Vector' as a large array of machine words. We
-- can also derive a 'Storable' instance similarly.
--
-- This is the most efficient encoding of an arbitrary finitary type, both due
-- to the asymptotics of encoding and decoding (logarithmic in @Cardinality a@
-- with base @Cardinality Word@) and the fact that word accesses are faster than
-- byte and bit accesses on almost all architectures. Unless you have concerns
-- regarding space, this encoding is a good choice.
--
-- Unless your type's cardinality is extremely large (a non-trivial multiple of
-- @Cardinality Word@), this encoding is wasteful. If your type's cardinality is
-- smaller than that of @Word@, you should consider "Data.Finitary.PackInto"
-- instead, as you will have much larger control over space usage at almost no
-- performance penalty. 
module Data.Finitary.PackWords 
(
  PackWords, pattern Packed
) where

import Data.Vector.Binary ()
import Data.Vector.Instances ()
import GHC.TypeNats
import Data.Proxy (Proxy(..))
import GHC.TypeLits.Extra
import CoercibleUtils (op, over, over2)
import Data.Kind (Type)
import Data.Finitary (Finitary(..))
import Data.Finite (Finite)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr, plusPtr)
import Numeric.Natural (Natural)
import Data.Hashable (Hashable(..))
import Control.DeepSeq (NFData(..))
import Control.Monad.Trans.State.Strict (evalState, get, modify, put)

import qualified Data.Binary as Bin
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

-- | An opaque wrapper around @a@, representing each value as a fixed-length
-- array of machine words.
newtype PackWords (a :: Type) = PackWords (VU.Vector Word)
  deriving (Eq, Ord, Show)

type role PackWords nominal

-- | To provide (something that resembles a) data constructor for 'PackWords', we
-- provide the following pattern. It can be used like any other data
-- constructor:
--
-- > import Data.Finitary.PackWords
-- >
-- > anInt :: PackWords Int
-- > anInt = Packed 10
-- >
-- > isPackedEven :: PackWords Int -> Bool
-- > isPackedEven (Packed x) = even x
--
-- __Every__ pattern match, and data constructor call, performs a
-- \(\Theta(\log_{\texttt{Cardinality Word}}(\texttt{Cardinality a}))\) encoding or decoding of @a@.
-- Use with this in mind.
pattern Packed :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  PackWords a -> a
pattern Packed x <- (packWords -> x)
  where Packed x = unpackWords x

instance Bin.Binary (PackWords a) where
  {-# INLINE put #-}
  put = Bin.put . op PackWords
  {-# INLINE get #-}
  get = PackWords <$> Bin.get

instance Hashable (PackWords a) where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt = hashWithSalt salt . op PackWords

instance NFData (PackWords a) where
  {-# INLINE rnf #-}
  rnf = rnf . op PackWords

instance (Finitary a, 1 <= Cardinality a) => Finitary (PackWords a) where
  type Cardinality (PackWords a) = Cardinality a
  {-# INLINE fromFinite #-}
  fromFinite = PackWords . intoWords
  {-# INLINE toFinite #-}
  toFinite = outOfWords . op PackWords

instance (Finitary a, 1 <= Cardinality a) => Bounded (PackWords a) where
  {-# INLINE minBound #-}
  minBound = start
  {-# INLINE maxBound #-}
  maxBound = end

instance (Finitary a, 1 <= Cardinality a) => Storable (PackWords a) where
  {-# INLINE sizeOf #-}
  sizeOf _ = wordLength @a * bytesPerWord
  {-# INLINE alignment #-}
  alignment _ = alignment (undefined :: Word)
  {-# INLINE peek #-}
  peek ptr = do let wordPtr = castPtr ptr
                PackWords <$> VU.generateM (wordLength @a) (peek . plusPtr wordPtr . (* bytesPerWord))
  {-# INLINE poke #-}
  poke ptr (PackWords v) = do let wordPtr = castPtr ptr
                              VU.foldM'_ go wordPtr v
    where go p e = poke p e >> pure (plusPtr p bytesPerWord) 

newtype instance VU.MVector s (PackWords a) = MV_PackWords (VU.MVector s Word)

instance (Finitary a, 1 <= Cardinality a) => VGM.MVector VU.MVector (PackWords a) where
  {-# INLINE basicLength #-}
  basicLength = over MV_PackWords ((`div` wordLength @a) . VGM.basicLength)
  {-# INLINE basicOverlaps #-}
  basicOverlaps = over2 MV_PackWords VGM.basicOverlaps
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over MV_PackWords (VGM.basicUnsafeSlice (i * wordLength @a) (len * wordLength @a))
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew len = MV_PackWords <$> VGM.basicUnsafeNew (len * wordLength @a)
  {-# INLINE basicInitialize #-}
  basicInitialize = VGM.basicInitialize . op MV_PackWords
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_PackWords v) i = fmap PackWords . VG.freeze . VGM.unsafeSlice (i * wordLength @a) (wordLength @a) $ v
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_PackWords v) i (PackWords x) = let slice = VGM.unsafeSlice (i * wordLength @a) (wordLength @a) v in
                                                        VG.unsafeCopy slice x

newtype instance VU.Vector (PackWords a) = V_PackWords (VU.Vector Word)

instance (Finitary a, 1 <= Cardinality a) => VG.Vector VU.Vector (PackWords a) where
  {-# INLINE basicLength #-}
  basicLength = over V_PackWords ((`div` wordLength @a) . VG.basicLength)
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = fmap V_PackWords . VG.basicUnsafeFreeze . op MV_PackWords
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = fmap MV_PackWords . VG.basicUnsafeThaw . op V_PackWords
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i len = over V_PackWords (VG.basicUnsafeSlice (i * wordLength @a) (len * wordLength @a))
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_PackWords v) i = pure . PackWords . VG.unsafeSlice (i * wordLength @a) (wordLength @a) $ v

instance (Finitary a, 1 <= Cardinality a) => VU.Unbox (PackWords a)

-- Helpers

type WordLength a = CLog (Cardinality Word) (Cardinality a)

{-# INLINE bitsPerWord #-}
bitsPerWord :: forall (a :: Type) . 
  (Num a) => 
  a
bitsPerWord = 8 * bytesPerWord

{-# INLINE bytesPerWord #-}
bytesPerWord :: forall (a :: Type) . 
  (Num a) => 
  a
bytesPerWord = fromIntegral . sizeOf $ (undefined :: Word)

{-# INLINE wordLength #-}
wordLength :: forall (a :: Type) (b :: Type) . 
  (Finitary a, 1 <= Cardinality a, Num b) => 
  b
wordLength = fromIntegral . natVal $ (Proxy :: Proxy (WordLength a))

{-# INLINE packWords #-}
packWords :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  a -> PackWords a
packWords = fromFinite . toFinite

{-# INLINE unpackWords #-}
unpackWords :: forall (a :: Type) . 
  (Finitary a, 1 <= Cardinality a) => 
  PackWords a -> a
unpackWords = fromFinite . toFinite

{-# INLINE intoWords #-}
intoWords :: forall (n :: Nat) . 
  (KnownNat n, 1 <= n) => 
  Finite n -> VU.Vector Word
intoWords = evalState (VU.replicateM (wordLength @(Finite n)) go) . fromIntegral @_ @Natural
  where go = do remaining <- get
                let (d, r) = quotRem remaining bitsPerWord
                put d >> pure (fromIntegral r)

{-# INLINE outOfWords #-}
outOfWords :: forall (n :: Nat) . 
  (KnownNat n) => 
  VU.Vector Word -> Finite n
outOfWords v = evalState (VU.foldM' go 0 v) 1
  where go old w = do power <- get
                      let placeValue = power * fromIntegral w
                      modify (* bitsPerWord)
                      return (old + placeValue)
