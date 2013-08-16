{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Shape
    ( Shape(..)
    , SomeShape(..)

    , Sphere(..)
    ) where

import Data.Colour (Colour, blue)
import Data.Material (Material)
import Data.Vec (Vec, Normalized, dot, normalize)
import Data.Ray (Ray(..))

class Shape a where
  intersect :: a -> Ray -> Maybe Double
  normalAt  :: a -> Vec -> Normalized Vec
  colourAt  :: a -> Vec -> Colour

  material  :: a -> Material
  material = undefined

data SomeShape = forall a. Shape a => SomeShape a

instance Shape SomeShape where
  intersect (SomeShape shape) = intersect shape
  normalAt (SomeShape shape)  = normalAt shape
  colourAt (SomeShape shape)  = colourAt shape
  material (SomeShape shape)  = material shape

-- * Shapes

data Sphere = Sphere
    { sphereCenter :: !Vec
    , sphereRadius :: !Double
    } deriving Show

instance Shape Sphere where
    intersect (Sphere { .. }) (Ray { .. }) =
        -- sphere equation:: (center - x)^2 == a^2
        -- ray equation::    (origin + t*d)
        -- combined and simplified:
        -- t^2 * d^2 -2t * d(center-origin) + (center-origin)^2 - a^2== 0
        -- at^2 + 2bt + c == 0
        if d < 0
        then Nothing
        else case filter (> 0) [t1, t2] of
         [] -> Nothing
         xs -> Just $! minimum xs
      where
        co = sphereCenter - rayOrigin
        a  = rayDirection `dot` rayDirection
        b  = rayDirection `dot` co
        c  = (co `dot` co) - sphereRadius*sphereRadius
        d  = b*b - a*c
        rd = sqrt d
        t1 = (b - rd) / a
        t2 = (b + rd) / a

    normalAt (Sphere { .. }) x = normalize (x - sphereCenter)

    colourAt _shpere = const blue
