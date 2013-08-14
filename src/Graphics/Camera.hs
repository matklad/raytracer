module Graphics.Camera
    ( Size(..)
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


type Size = (Double, Double)

type Resolution = (Int, Int)

-- up, right
type Orientation = (Normalized (Vec Double), Normalized (Vec Double))

data Camera = Camera { location          :: !(Vec Double)
                     , direction         :: !(Normalized (Vec Double))
                     , focus             :: !Double
                     , screenOrientation :: !Orientation
                     , screenSize        :: !Size
                     , screenResolution  :: !Resolution
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
    orient = (normalize up, normalize $ dir `cross` up)

sizeW :: Camera -> Double
sizeW cam = fst $ screenSize cam

sizeH :: Camera -> Double
sizeH cam = snd $ screenSize cam

resolutionW :: Camera -> Int
resolutionW cam = fst $ screenResolution cam

resolutionH :: Camera -> Int
resolutionH cam = snd $ screenResolution cam

screenUp :: Camera -> Normalized (Vec Double)
screenUp cam = fst $ screenOrientation cam

screenRight :: Camera -> Normalized (Vec Double)
screenRight cam = snd $ screenOrientation cam

applyCamera :: Camera -> Int -> Int -> Ray Double
applyCamera cam x y = Ray origin (normalize dir)
  where
    mw = fromIntegral (resolutionW cam) / 2.0
    mh = fromIntegral (resolutionH cam) / 2.0
    shiftUp = (fromIntegral y - mh) * (sizeH cam) / mh
    shiftRight = (fromIntegral x - mw) * (sizeW cam) / mw
    dir = focus cam `scale` direction cam +
          shiftUp `scale` screenUp cam +
          shiftRight `scale` screenRight cam
    origin = location cam + dir
