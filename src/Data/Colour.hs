{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Colour
    ( Colour
    , rgb
    , toGL
    , toRGB8
    , red
    , green
    , blue
    , black
    , white
    ) where

import Foreign.C.Types (CDouble(..))
import qualified Graphics.Rendering.OpenGL as GL
import Data.Vec (Vec, vec, cev)
import Data.Word (Word8)

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

toGL :: Colour -> GL.Color3 GL.GLdouble
toGL c = GL.Color3 (CDouble r) (CDouble g) (CDouble b) where
  !(r, g, b) = cev c

toRGB8 :: Colour -> (Word8, Word8, Word8)
toRGB8 c = (to r, to g, to b)
  where
    (r, g, b) = cev c
    to x = round $ max (min (x * 255) 255) 0
{-# INLINE toGL #-}
