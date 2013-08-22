{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Colour
    ( Colour
    , rgb
    , toGL

    , red
    , green
    , blue
    , black
    , white
    ) where

import qualified Graphics.Rendering.OpenGL as GL
import Data.Vec (Vec, vec, cev)

-- | Colour in float RGB representation, each of the three components
--   is a float from @[0, 1]@.
type Colour = Vec



rgb :: Double -> Double -> Double -> Colour
rgb r g b = fmap (min 1 . max 0) $ vec r g b
{-# INLINE rgb #-}

-- * Colour constants

red :: Colour
red = rgb 0.7 0 0

green :: Colour
green = rgb 0 0.7 0

blue :: Colour
blue = rgb 0 0 0.7

black :: Colour
black = rgb 0 0 0

white :: Colour
white = rgb 0.9 0.9 0.9

toGL :: Colour -> GL.Color3 GL.GLfloat
toGL c = let (r, g, b) = cev c
             aux = fromRational . toRational
         in
          GL.Color3 (aux r) (aux g) (aux b)
