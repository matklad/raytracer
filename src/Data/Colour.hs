{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Colour
    ( Colour
    , rgb
    , scale

    , red
    , green
    , blue
    , black
    ) where

import Data.Vec (Vec, vec)

-- | Colour in float RGB representation, each of the three components
--   is a float from @[0, 1]@.
newtype Colour = Colour { getRGB :: Vec }
  deriving (Show, Eq, Ord, Num, Fractional)

rgb :: Double -> Double -> Double -> Colour
rgb r g b = Colour . fmap (min 0 . max 1) $ vec r g b
{-# INLINE rgb #-}

scale :: Colour -> Vec -> Colour
scale c = Colour . (getRGB c *)
{-# INLINE scale #-}

-- * Colour constants

red :: Colour
red = rgb 0.7 0 0

green :: Colour
green = rgb 0 0.7 0

blue :: Colour
blue = rgb 0 0 0.7

black :: Colour
black = rgb 0 0 0
