{-# LANGUAGE RecordWildCards #-}

module Graphics.Light
    ( LightSource(..)
    , PointSource(..) 
    ) where

import Data.Colour(Colour, white)
import Data.Vec(Vec, Normalized, dot, normalize)

class LightSource a where
    colour :: a -> Colour
    colour = const white

    shade :: a -> Vec -> Normalized Vec -> (Normalized Vec, Double)


data PointSource = PointSource
    { pointSourcePosition :: !Vec
    , pointSourceColour   :: !Colour
    } deriving Show


instance LightSource PointSource where
    colour = pointSourceColour
    shade (PointSource { .. }) v n = (direction, intensivity)
      where
        direction   = normalize $ v - pointSourcePosition
        intensivity = direction `dot` n 

        
