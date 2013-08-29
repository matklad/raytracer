module Graphics.Ray.Types.Material
    ( Material (..)
    , simpleMaterial
    ) where

import Data.Colour (Colour, white)

data Material = Material
    { materialAmbient  :: !Colour
    , materialDiffuse  :: !Colour
    , materialSpecular :: !Colour
    , materialPhong    :: !Double
    } deriving Show

simpleMaterial :: Material
simpleMaterial = Material white white white 4
