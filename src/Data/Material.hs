module Data.Material
    ( Material
    , simpleMaterial
    , materialAmbient
    ) where

import Data.Colour (Colour, white)

data Material = Material
    { materialAmbient :: !Colour
    } deriving Show

simpleMaterial :: Material
simpleMaterial = Material white
