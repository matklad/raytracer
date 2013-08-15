{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Shape
    ( Shape(..)
    , SomeShape(..)

    , Sphere(..)
    ) where

import Data.Colour (Colour, green, blue)
import Data.Material (Material)
import Data.Vec (VecD, Normalized, dot, normalize)
import Data.Ray (RayD, Ray(..))

class Shape a where
  intersect :: a -> RayD -> Maybe Double
  normal    :: a -> VecD -> Normalized VecD
  colour :: a -> Colour
  colour _ = green

  material :: a -> Material
  material = undefined

data SomeShape = forall a. Shape a => SomeShape a

-- * Shapes

data Sphere = Sphere
    { sphereCenter :: !(VecD)
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

    normal (Sphere { .. }) x = normalize  (x - sphereCenter)

    colour _ = blue
