{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Vec.Internal
    ( Vec(..)
    , Normalized
    , norm

    , scale
    , normalize
    , dot
    , cross
    , lowerBound
    , upperBound
    , unzero
    ) where

import Prelude hiding (sum, zipWith)

import Data.Foldable (Foldable(..), sum)
import Data.List (foldl1')
import Data.Monoid (Monoid(..), Sum, Product)

import Control.DeepSeq (NFData(..))

-- | A vector in R^3.
data Vec a = Vec { vecX :: !a
                 , vecY :: !a
                 , vecZ :: !a
                 }
  deriving (Eq, Ord, Show)

type Normalized a = a


instance Functor Vec where
  fmap f (Vec { .. }) = Vec { vecX = f vecX, vecY = f vecY, vecZ = f vecZ }

instance Foldable Vec where
  foldl f acc (Vec { .. }) = f (f (f acc vecZ) vecY) vecX
  foldr f acc (Vec { .. }) = f vecX (f vecY (f vecZ acc))

instance Monoid a => Monoid (Vec a) where
  mempty  = singleton mempty
  mappend = zipWith mappend
  {-# SPECIALIZE instance Monoid (Vec (Sum Double)) #-}
  {-# SPECIALIZE instance Monoid (Vec (Product Double)) #-}

instance Num a => Num (Vec a) where
  u + v = zipWith (+) u v
  u - v = zipWith (-) u v
  u * v = zipWith (*) u v
  negate  = fmap negate
  abs     = fmap abs
  signum  = fmap signum
  fromInteger  = singleton . fromInteger
  {-# SPECIALIZE instance Num (Vec Double) #-}

instance Fractional a => Fractional (Vec a) where
  u / v = zipWith (/) u v
  recip = fmap recip
  fromRational = singleton . fromRational
  {-# SPECIALIZE instance Fractional (Vec Double) #-}

instance NFData a => NFData (Vec a) where
    rnf (Vec a b c) = rnf a `seq` rnf b `seq` rnf c

-- | Creates a vector with same value for all components.
singleton :: a -> Vec a
singleton c = Vec { vecX = c, vecY = c, vecZ = c}
{-# INLINEABLE singleton #-}
{-# SPECIALIZE INLINE singleton :: Double -> Vec Double #-}

-- | Computes L2-norm of the vector.
norm :: (Floating a, Num a) => Vec a -> a
norm v = sqrt $! dot v v
{-# INLINEABLE norm #-}
{-# SPECIALIZE INLINE norm :: Vec Double -> Double #-}

-- | Scales a vector by a constant factor @f@.
scale :: Num a => a -> Vec a -> Vec a
f `scale` v = fmap (* f) v
{-# INLINEABLE scale #-}
{-# SPECIALIZE INLINE scale :: Double -> Vec Double -> Vec Double #-}

-- | Computes a unit vector, in the direction of @v@.
normalize :: Floating a => Vec a -> Normalized (Vec a)
normalize v = fmap (/ norm v) v
{-# INLINEABLE normalize #-}
{-# SPECIALIZE INLINE normalize :: Vec Double -> Normalized (Vec Double) #-}

-- | Computes the dot product of two vectors.
dot :: Num a => Vec a -> Vec a -> a
u `dot` v = sum (u * v) where
{-# INLINEABLE dot #-}
{-# SPECIALIZE INLINE dot :: Vec Double -> Vec Double -> Double #-}

-- | Computes the cross product of two vectors.
cross :: Num a => Vec a -> Vec a -> Vec a
cross u v = Vec { vecX = sX, vecY = sY, vecZ = sZ } where
  !sX = vecY u * vecZ v - vecZ u * vecY v
  !sY = vecZ u * vecX v - vecX u * vecZ v
  !sZ = vecX u * vecY v - vecY u * vecX v
{-# INLINEABLE cross #-}
{-# SPECIALIZE INLINE cross :: Vec Double -> Vec Double -> Vec Double #-}

lowerBound :: Ord a => [Vec a] -> Vec a
lowerBound = foldl1' (zipWith min)
{-# INLINEABLE lowerBound #-}
{-# SPECIALIZE INLINE lowerBound :: [Vec Double] -> Vec Double  #-}

upperBound :: Ord a => [Vec a] -> Vec a
upperBound = foldl1' (zipWith max)
{-# INLINEABLE upperBound #-}
{-# SPECIALIZE INLINE upperBound :: [Vec Double] -> Vec Double  #-}

unzero :: (Fractional a, Ord a) => Vec a -> Vec a
unzero = fmap aux
  where
    aux x = if abs x > 0.0000001
            then x
            else signum x * 0.0000001
{-# INLINEABLE unzero #-}
{-# SPECIALIZE INLINE unzero :: Vec Double -> Vec Double  #-}


-- | Combines two vectors using a binary function.
zipWith :: (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipWith (f :: a -> b -> c) u v =
    Vec { vecX = g vecX, vecY = g vecY, vecZ = g vecZ }
  where
    g :: (forall d. Vec d -> d) -> c
    g field = field u `f` field v
    {-# INLINE g #-}
{-# INLINABLE zipWith #-}
{-# SPECIALIZE INLINE
    zipWith :: (Double -> Double -> Double)
            -> Vec Double
            -> Vec Double
            -> Vec Double #-}
