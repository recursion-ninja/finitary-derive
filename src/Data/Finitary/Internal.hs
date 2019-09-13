module Data.Finitary.Internal where

import Data.Bool (bool)
import Data.Finitary (Finitary(..))
import Control.Monad.Fail (MonadFail(..))

{-# INLINE decodeWith #-}
decodeWith :: (MonadFail m, Show n, Integral n, Finitary a) => (a -> f a) -> n -> n -> m (f a)
decodeWith g card ix = bool (Control.Monad.Fail.fail errorMessage) (pure . g . fromFinite . fromIntegral $ ix) (ix <= card)
  where errorMessage = "Cardinality " ++ show card ++ " cannot fit index: " ++ show ix
