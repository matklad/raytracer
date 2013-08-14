module Graphics.Camera where

import Data.Vec (Vec, Normalized, normalize, cross)

data Data2D a = Data2D { dataX :: !a
                       , dataY :: !a
                       }
    deriving (Eq, Show)

type Size = Data2D Double

type Resolution = Data2D Int

-- up, right
type Orientation = Data2D (Normalized (Vec Double))

data Camera = Camera { location          :: Vec Double
                     , direction         :: Normalized (Vec Double)
                     , focus             :: Double
                     , screenOrientation :: Orientation
                     , screenSize        :: Size
                     , screenResolution  :: Resolution
                     }
  deriving (Eq, Show)

-- location, lookAt, up, focus, size, resolution
mkCamera :: Vec Double -> Vec Double -> Vec Double ->
            Double -> Size -> Resolution -> Camera
mkCamera loc lookAt up focus size res =
    Camera loc dir focus orient size res
  where
    dir = normalize $ lookAt - loc
    nup = normalize up
    orient = Data2D (normalize up) (normalize $ dir `cross` up)
