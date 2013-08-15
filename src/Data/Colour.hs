{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Colour
    ( Colour
    , rgb

    , red
    , green
    , blue
    ) where

import Data.Vec (Vec(..))

-- | Colour in float RGB representation, each of the three components
--   is a float from @[0, 1]@.
newtype Colour = Colour (Vec Double)
  deriving (Show, Eq, Ord, Num, Fractional)

rgb :: Double -> Double -> Double -> Colour
rgb r g b = Colour $ fmap (min 0 . max 1) v where
  v :: Vec Double
  v = Vec { vecX = r, vecY = g, vecZ = b }
{-# INLINE rgb #-}

red :: Colour
red = rgb 0.7 0 0

green :: Colour
green = rgb 0 0.7 0

blue :: Colour
blue = rgb 0 0 0.7
