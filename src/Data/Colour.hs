{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Colour
    ( Colour
    , red
    , green
    , blue
    ) where

import Data.Vec (Vec(..))

-- | Colour representated as float RGB.
newtype Colour = Colour (Vec Double)
  deriving (Show, Eq, Num, Fractional)



mkColour :: Double -> Double -> Double -> Colour
mkColour r g b = Colour $ Vec { vecX = r, vecY = g, vecZ = b}

red :: Colour
red = mkColour 0.7 0 0

green :: Colour
green = mkColour 0 0.7 0

blue :: Colour
blue = mkColour 0 0 0.7


