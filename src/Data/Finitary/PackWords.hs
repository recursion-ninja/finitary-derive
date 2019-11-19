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

module Data.Finitary.PackWords 
(
  PackWords, pattern Packed
) where

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
import Control.Monad.Trans.State.Strict (evalState, get, modify)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

newtype PackWords (a :: Type) = PackWords (VU.Vector Word)
  deriving (Eq, Ord)

type role PackWords nominal

pattern Packed :: forall (a :: Type) . 
  (Finitary a) => 
  PackWords a -> a
pattern Packed x <- (packWords -> x)
  where Packed x = unpackWords x

instance Hashable (PackWords a) where
  hashWithSalt salt = hashWithSalt salt . op PackWords

instance NFData (PackWords a) where
  rnf = rnf . op PackWords

instance (Finitary a) => Finitary (PackWords a) where
  type Cardinality (PackWords a) = Cardinality a
  fromFinite = PackWords . intoWords
  toFinite = outOfWords . op PackWords

instance (Finitary a, 1 <= Cardinality a) => Bounded (PackWords a) where
  minBound = start
  maxBound = end

instance (Finitary a, 1 <= Cardinality a) => Storable (PackWords a) where
  sizeOf _ = wordLength @a * bytesPerWord
  alignment _ = alignment (undefined :: Word)
  peek ptr = do let wordPtr = castPtr ptr
                PackWords <$> VU.generateM (wordLength @a) (peek . plusPtr wordPtr . (* bytesPerWord))
  poke ptr (PackWords v) = do let wordPtr = castPtr ptr
                              VU.foldM'_ go wordPtr v
    where go p e = poke p e >> pure (plusPtr p bytesPerWord) 

newtype instance VU.MVector s (PackWords a) = MV_PackWords (VU.MVector s Word)

instance (Finitary a, 1 <= Cardinality a) => VGM.MVector VU.MVector (PackWords a) where
  basicLength = over MV_PackWords ((`div` wordLength @a) . VGM.basicLength)
  basicOverlaps = over2 MV_PackWords VGM.basicOverlaps
  basicUnsafeSlice i len = over MV_PackWords (VGM.basicUnsafeSlice (i * wordLength @a) (len * wordLength @a))
  basicUnsafeNew len = MV_PackWords <$> VGM.basicUnsafeNew (len * wordLength @a)
  basicInitialize = VGM.basicInitialize . op MV_PackWords
  basicUnsafeRead (MV_PackWords v) i = fmap PackWords . VG.freeze . VGM.unsafeSlice (i * wordLength @a) (wordLength @a) $ v
  basicUnsafeWrite (MV_PackWords v) i (PackWords x) = let slice = VGM.unsafeSlice (i * wordLength @a) (wordLength @a) v in
                                                        VG.unsafeCopy slice x

newtype instance VU.Vector (PackWords a) = V_PackWords (VU.Vector Word)

instance (Finitary a, 1 <= Cardinality a) => VG.Vector VU.Vector (PackWords a) where
  basicLength = over V_PackWords ((`div` wordLength @a) . VG.basicLength)
  basicUnsafeFreeze = fmap V_PackWords . VG.basicUnsafeFreeze . op MV_PackWords
  basicUnsafeThaw = fmap MV_PackWords . VG.basicUnsafeThaw . op V_PackWords
  basicUnsafeSlice i len = over V_PackWords (VG.basicUnsafeSlice (i * wordLength @a) (len * wordLength @a))
  basicUnsafeIndexM (V_PackWords v) i = pure . PackWords . VG.unsafeSlice (i * wordLength @a) (wordLength @a) $ v

instance (Finitary a, 1 <= Cardinality a) => VU.Unbox (PackWords a)

-- Helpers

type WordLength a = CLog (Cardinality Word) (Cardinality a)

bitsPerWord :: forall (a :: Type) . 
  (Num a) => 
  a
bitsPerWord = 8 * bytesPerWord

bytesPerWord :: forall (a :: Type) . 
  (Num a) => 
  a
bytesPerWord = fromIntegral . sizeOf $ (undefined :: Word)

wordLength :: forall (a :: Type) (b :: Type) . 
  (Finitary a, 1 <= Cardinality a, Num b) => 
  b
wordLength = fromIntegral . natVal $ (Proxy :: Proxy (WordLength a))

packWords :: forall (a :: Type) . 
  (Finitary a) => 
  a -> PackWords a
packWords = fromFinite . toFinite

unpackWords :: forall (a :: Type) . 
  (Finitary a) => 
  PackWords a -> a
unpackWords = fromFinite . toFinite

intoWords :: forall (n :: Nat) . 
  (KnownNat n) => 
  Finite n -> VU.Vector Word
intoWords = VU.unfoldr go . fromIntegral @_ @Natural
  where go 0 = Nothing
        go n = let (d, r) = quotRem n bitsPerWord in
                Just (fromIntegral r, d)

outOfWords :: forall (n :: Nat) . 
  (KnownNat n) => 
  VU.Vector Word -> Finite n
outOfWords v = evalState (VU.foldM' go 0 v) 1
  where go old w = do power <- get
                      let placeValue = power * fromIntegral w
                      modify (* bitsPerWord)
                      return (old + placeValue)
