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

mkCamera :: Vec    -- ^ camera location
         -> Vec    -- ^ point in the scene
         -> Vec    -- ^ up
         -> Double -- ^ distance to the screen
         -> Size
         -> Resolution
         -> Camera
mkCamera loc lookAt up focus = Camera loc dir focus orient where
  dir    = normalize $ lookAt - loc
  right  = normalize $ dir `cross` up
  up'    = normalize $ right `cross` dir
  orient = (up', right)

applyCamera :: Camera -> (Int, Int) -> Ray
applyCamera (Camera { .. }) (x, y) = Ray { .. }
  where
    (resolutionW, resolutionH) = camScreenResolution
    (sizeW, sizeH) = camScreenSize
    (screenUp, screenRight) = camScreenOrientation
    mw = fromIntegral resolutionW
    mh = fromIntegral resolutionH
    shiftUp    = (fromIntegral y - mh / 2.0) * sizeH  / mh
    shiftRight = (fromIntegral x - mw / 2.0) * sizeW / mw
    rayOrigin    = camLocation + rayDirection
    rayDirection = normalize (camFocus `scale` camDirection +
                              shiftUp `scale` screenUp +
                              shiftRight `scale` screenRight)
