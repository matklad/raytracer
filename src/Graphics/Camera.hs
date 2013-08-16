{-# LANGUAGE RecordWildCards #-}

module Graphics.Camera
    ( Size
    , Resolution
    , Orientation
    , Camera(..)
    , mkCamera
    , applyCamera
    ) where

import Data.Vec (Vec, Normalized, normalize, scale, cross)
import Data.Ray (Ray(..))


type Size = (Double, Double)

type Resolution = (Int, Int)

type Orientation = ( Normalized Vec   -- ^ up
                   , Normalized Vec   -- ^ right
                   )

data Camera = Camera
    { camLocation          :: !Vec
    , camDirection         :: !(Normalized Vec)
    , camFocus             :: !Double
    , camScreenOrientation :: !Orientation
    , camScreenSize        :: !Size
    , camScreenResolution  :: !Resolution
    } deriving (Eq, Show)

mkCamera :: Vec  -- camera location
         -> Vec  -- point on the screen
         -> Vec  -- up
         -> Double      -- distance to the screen
         -> Size
         -> Resolution
         -> Camera
mkCamera loc lookAt up focus size res =
    Camera loc dir focus orient size res
  where
    dir    = normalize $ lookAt - loc
    orient = (normalize up, normalize $ dir `cross` up)

applyCamera :: Camera -> (Int, Int) -> Ray
applyCamera (Camera { .. }) (x, y) = Ray { .. }
  where
    (resolutionW, resolutionH) = camScreenResolution
    (sizeW, sizeH) = camScreenSize
    (screenUp, screenRight) = camScreenOrientation
    mw = fromIntegral resolutionW / 2.0
    mh = fromIntegral resolutionH / 2.0
    shiftUp    = (fromIntegral y - mh) * sizeH  / mh
    shiftRight = (fromIntegral x - mw) * sizeW / mw
    rayOrigin    = camLocation + rayDirection
    rayDirection = normalize $ (camFocus `scale` camDirection +
                                shiftUp `scale` screenUp +
                                shiftRight `scale` screenRight)
