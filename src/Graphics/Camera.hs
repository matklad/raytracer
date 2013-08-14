module Graphics.Camera
    ( Data2D(..)
    , Size(..)
    , Resolution(..)
    , Orientation(..)
    , Camera(..)
    ,  mkCamera

    , sizeW
    , sizeH
    , resolutionW
    , resolutionH
    , screenUp
    , screenRight
    , applyCamera
    ) where

import Data.Vec (Vec(..), Normalized(..), normalize, scale, cross)
import Data.Ray (Ray(..))

data Data2D a = Data2D { dataX :: !a
                       , dataY :: !a
                       }
    deriving (Eq, Show)

type Size = Data2D Double

type Resolution = Data2D Int

-- up, right
type Orientation = Data2D (Normalized (Vec Double))

data Camera = Camera { location          :: !(Vec Double)
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

sizeW :: Camera -> Double
sizeW cam = dataX $ screenSize cam

sizeH :: Camera -> Double
sizeH cam = dataY $ screenSize cam

resolutionW :: Camera -> Int
resolutionW cam = dataX $ screenResolution cam

resolutionH :: Camera -> Int
resolutionH cam = dataY $ screenResolution cam

screenUp :: Camera -> Normalized (Vec Double)
screenUp cam = dataX $ screenOrientation cam

screenRight :: Camera -> Normalized (Vec Double)
screenRight cam = dataY $ screenOrientation cam

applyCamera :: Camera -> Int -> Int -> Ray Double
applyCamera cam x y = Ray origin (normalize dir)
  where
    mw = fromIntegral (resolutionW cam) / 2.0
    mh = fromIntegral (resolutionH cam) / 2.0
    shiftUp = (fromIntegral y - mh) * (sizeH cam) / mh
    shiftRight = (fromIntegral x - mw) * (sizeW cam) / mw
    dir = (focus cam) `scale` (direction cam) +
          shiftUp `scale` (screenUp cam) +
          shiftRight `scale` (screenRight cam)
    origin = (location cam) + dir
