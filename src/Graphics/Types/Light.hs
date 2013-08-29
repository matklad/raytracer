{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Types.Light
    ( LightSource(..)
    , PointSource(..)
    , SomeLight(..)
    , Light(..)
    ) where

import Data.Colour(Colour, white)
import Data.Vec(Vec, Normalized, dot, normalize, norm)

data Light = Light
  { lightDirection :: !Vec
  , lightColour    :: !Colour
  , lightPower     :: !Double
  , lightDistance  :: !Double
  }

class LightSource a where
    colour :: a -> Colour
    colour = const white

    shed :: a -> Vec -> Normalized Vec -> Light

data SomeLight = forall a. LightSource a => SomeLight a

instance LightSource SomeLight where
    colour (SomeLight l) = colour l
    shed (SomeLight l)  = shed l

data PointSource = PointSource
    { pointSourcePosition :: !Vec
    , pointSourceColour   :: !Colour
    } deriving Show


instance LightSource PointSource where
    colour = pointSourceColour
    shed (PointSource { .. }) p n = Light { .. }
      where
        path = pointSourcePosition - p
        lightDirection = normalize $ path
        lightColour    = pointSourceColour
        lightPower     = max 0 $ lightDirection `dot` n
        lightDistance  = norm path
