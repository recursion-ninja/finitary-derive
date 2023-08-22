{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GADTs                      #-}

module Data.Finitary.Coercion
  ( op
  , over
  , over2
  ) where

-- base
import Data.Coerce
--import GHC.Generics
--import GHC.TypeLits (TypeError, ErrorMessage (..))
--import Data.Kind (Constraint)


{- | Reverse the type of a "packer".

>>> op All (All True)
True
>>> op (Identity . Sum) (Identity (Sum 3))
3
-}
{-# INLINE op #-}
op :: Coercible a b
   => (a `to` b)
   -> b
   -> a
op _ = coerce


{- |
Take a function which works on the underlying types, and switch it to a function
that works on the newtypes.
@
over _ f = pack . f . unpack
@

>>> over All not (All False)
All {getAll = True}
-}
over
    ::
    ( Coercible n  o
    , Coercible n' o'
    )
    => (o `to` n)
    -> (o -> o')
    -> (n -> n')
over _ f = coerce #. f .# coerce


-- | The opposite of 'under2'.
--
-- @
-- over2 _ f n0 n1 = pack $ f (unpack n0) (unpack n1)
-- @
over2 :: (Coercible n o, Coercible n' o', Coercible n'' o'')
       => (o `to` n) -> (o -> o' -> o'') -> (n -> n' -> n'')
over2 _ = coerce

-- | Coercive left-composition.
--
-- >>> (All #. not) True
-- All {getAll = False}
--
-- The semantics with respect to bottoms are:
--
-- @
-- p '#.' ⊥ ≡ ⊥
-- p '#.' f ≡ p '.' f
-- @
infixr 9 #.
(#.) :: Coercible b c => (b `to` c) -> (a -> b) -> a -> c
(#.) _ = coerce
{-# INLINE (#.) #-}

-- | Coercive right-composition.
--
-- >>> (stimes 2 .# Product) 3
-- Product {getProduct = 9}
--
-- The semantics with respect to bottoms are:
--
-- @
-- ⊥ '.#' p ≡ ⊥
-- f '.#' p ≡ p '.' f
-- @
infixr 9 .#
(.#) :: Coercible a b => (b -> c) -> (a `to` b) -> a -> c
(.#) f _ = coerce f
{-# INLINE (.#) #-}
