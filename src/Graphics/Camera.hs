module Graphics.Camera where

import Data.Vec

data ScreenSize = SSize { width  :: Double
                        , height :: Double
                        }
    deriving (Eq, Show)

data PixelMatrix = PMat { x :: Int
                        , y :: Int
                        }
    deriving (Eq, Show)

data ScreenOrientation = SOrient { up   :: Normalized (Vec Double)
                                 , left :: Normalized (Vec Double)
                                 }
    deriving (Eq, Show)

data Screen = Screen ScreenSize PixelMatrix ScreenOrientation
    deriving (Eq, Show)

data Camera = Camera { location  :: Vec Double
                     , direction :: Normalized (Vec Double)
                     , focus     :: Double
                     , screen    :: Screen
                     }
    deriving (Eq, Show)


-- Location, LookAt, Up, Focus, Width, Height, x, y
constructCamera :: Vec Double -> Vec Double -> Vec Double ->
                   Double -> Double -> Double ->
                   Int -> Int -> Camera
constructCamera location lookAt up focus width height x y =
    Camera location ndirection focus
           (Screen (SSize width height) (PMat x y) (SOrient nup nright))
  where
    direction = lookAt - location
    right = direction `cross` up
    ndirection = normalize direction
    nup = normalize up
    nright = normalize right
