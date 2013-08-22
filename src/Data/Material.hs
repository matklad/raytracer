module Data.Material
    ( Material
    , simpleMaterial
    , materialAmbient
    , materialDiffuse
    ) where

import Data.Colour (Colour, white)

data Material = Material
    { materialAmbient :: !Colour
    , materialDiffuse :: !Colour
    } deriving Show

simpleMaterial :: Material
simpleMaterial = Material white white
