{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Light
    ( LightSource(..)
    , PointSource(..) 
    ) where

import Data.Colour(Colour, white)
import Data.Vec(Vec, Normalized, dot, normalize)

data Light = Light
  { lightDirection :: !Vec
  , lightColour    :: !Colour
  , lightPower     :: !Double
  }

class LightSource a where
    colour :: a -> Colour
    colour = const white

    shade :: a -> Vec -> Normalized Vec -> Light

data SomeLight = forall a. LightSource a => SomeLight a

instance LightSource SomeLight where
    colour (SomeLight l) = colour l
    shade (SomeLight l)  = shade l

data PointSource = PointSource
    { pointSourcePosition :: !Vec
    , pointSourceColour   :: !Colour
    } deriving Show


instance LightSource PointSource where
    colour = pointSourceColour
    shade (PointSource { .. }) point norm = Light { .. }
      where
        lightDirection = normalize $ pointSourcePosition - point
        lightColour    = pointSourceColour
        lightPower     = max 0 $ lightDirection `dot` norm

        
