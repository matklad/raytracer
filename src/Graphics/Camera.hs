module Graphics.Camera
    ( ScreenSize
    , ScreenOrientation(..)
    , PixelMatrix(..)
    , Screen(..)
    , Camera(..)

    , constructCamera
    ) where

import Data.Vec (Vec(..), Normalized(..), cross, normalize)

data PixelMatrix = PixelMatrix { pmX :: !Int, pmY :: !Int }
  deriving (Eq, Show)

type ScreenSize = (Double, Double)
data ScreenOrientation = ScreenOrientation
    { soUp   :: !(Normalized (Vec Double))
    , soLeft :: !(Normalized (Vec Double))
    } deriving (Eq, Show)

data Screen = Screen
    { screenSize        :: !ScreenSize
    , screenPM          :: !PixelMatrix
    , screenOrientation :: !ScreenOrientation
    } deriving (Eq, Show)

data Camera = Camera
    { camLocation  :: !(Vec Double)
    , camDirection :: !(Normalized (Vec Double))
    , camFocus     :: !Double
    , camScreen    :: !Screen
    } deriving (Eq, Show)

-- Location, LookAt, Up, Focus, Width, Height, x, y
constructCamera :: Vec Double -> Vec Double -> Vec Double ->
                   Double -> Double -> Double ->
                   Int -> Int -> Camera
constructCamera location lookAt up focus width height x y =
    Camera location ndirection focus screen
  where
    screen = Screen { screenSize = (width, height)
                    , screenPM   = PixelMatrix x y
                    , screenOrientation = ScreenOrientation nup nright
                    }
    direction = lookAt - location
    right = direction `cross` up
    ndirection = normalize direction
    nup = normalize up
    nright = normalize right
