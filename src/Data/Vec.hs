{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Data.Vec
  ( Vec(..)
  , singleton

  , dot
  , cross
  , zipWith
  ) where

import Prelude hiding (zipWith)

import Data.Monoid (Monoid(..))

-- | A vector in R^3.
data Vec a = Vec { vecX :: !a
                 , vecY :: !a
                 , vecZ :: !a
                 }
  deriving (Eq, Ord, Show)

instance Functor Vec where
  fmap f (Vec { .. }) = Vec { vecX = f vecX, vecY = f vecY, vecZ = f vecZ }

instance Monoid a => Monoid (Vec a) where
  mempty  = singleton mempty
  mappend = zipWith mappend

instance Num a => Num (Vec a) where
  u + v = zipWith (+) u v
  u - v = zipWith (-) u v
  u * v = zipWith (*) u v
  negate  = fmap negate
  abs     = fmap abs
  signum  = fmap signum
  fromInteger  = singleton . fromInteger

instance Fractional a => Fractional (Vec a) where
  u / v = zipWith (/) u v
  recip = fmap recip
  fromRational = singleton . fromRational

-- | Creates a vector with same value for all components.
singleton :: a -> Vec a
singleton !c = Vec { vecX = c, vecY = c, vecZ = c}
{-# INLINE singleton #-}

-- | Computes the dot product of two vectors.
dot :: Num a => Vec a -> Vec a -> a
u `dot` v = vecX + vecY + vecZ where
  Vec { .. } = u * v
{-# INLINABLE dot #-}

-- | Computes the cross product of two vectors.
cross :: Num a => Vec a -> Vec a -> Vec a
cross u v = Vec { vecX = sX, vecY = sY, vecZ = sZ } where
  !sX = vecY u * vecZ v - vecZ u * vecY v
  !sY = vecZ u * vecX v - vecX u * vecZ v
  !sZ = vecX u * vecY v - vecY u * vecX v
{-# INLINABLE cross #-}

-- | Combines two vectors using a binary function.
zipWith :: (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipWith (f :: a -> b -> c) u v =
    Vec { vecX = g vecX, vecY = g vecY, vecZ = g vecZ }
  where
    g :: (forall d. Vec d -> d) -> c
    g field = field u `f` field v
    {-# INLINE g #-}
{-# INLINABLE zipWith #-}
