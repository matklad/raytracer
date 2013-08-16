{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Shape
    ( Shape(..)
    , SomeShape(..)

    , Texture(..)

    , Sphere(..)
    , triangle
    ) where

import Data.Colour (Colour)
import Data.Vec (Vec, Normalized, dot, normalize, scale, cross)
import Data.Ray (Ray(..))



data Texture = Solid Colour
             | Procedure (Vec -> Colour)
             | Bitmap

class Shape a where
    intersect :: a -> Ray -> Maybe Double
    normalAt  :: a -> Vec -> Normalized Vec
    texture   :: a -> Texture

    colourAt :: a -> Vec -> Colour
    colourAt = go . texture where
      go (Solid c)     = const c
      go (Procedure f) = f
      go _t            = undefined
    {-# INLINE colourAt #-}

data SomeShape = forall a. Shape a => SomeShape a

instance Shape SomeShape where
    intersect (SomeShape shape) = intersect shape
    normalAt (SomeShape shape)  = normalAt shape
    texture (SomeShape shape)   = texture shape

-- * Shapes

data Sphere = Sphere
    { sphereCenter  :: !Vec
    , sphereRadius  :: !Double
    , sphereTexture :: Texture
    }

instance Shape Sphere where
    intersect (Sphere { .. }) (Ray { .. }) =
        -- sphere equation:: (C - X)^2 == r^2
        -- ray equation::    (O + t*D)
        -- combined and simplified:
        -- t^2 * D^2 -2t * D(C-O) + (C-O)^2 - r^2== 0
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
    texture = sphereTexture


data Triangle = Triangle
    { triangleA :: !Vec
    , triangleB :: !Vec
    , triangleC :: !Vec
    , triangleTexture :: Texture
      -- microoptimization: precomputed constants
    , triangleAB   :: !Vec
    , triangleAC   :: !Vec
    , triangleN    :: !Vec
    , triangleABxN :: !Vec
    , triangleACxN :: !Vec
    , triangleABdACxN :: !Double
    , triangleACdABxN :: !Double
    }

triangle :: Texture -> Vec -> Vec -> Vec -> Triangle
triangle triangleTexture triangleA triangleB triangleC =
    Triangle { .. }
  where
    triangleAB = triangleB - triangleA
    triangleAC = triangleC - triangleA
    triangleN  = normalize $ triangleAB `cross` triangleAC
    triangleABxN = triangleAB `cross` triangleN
    triangleACxN = triangleAC `cross` triangleN
    triangleABdACxN = triangleAB `dot` triangleACxN
    triangleACdABxN = triangleAC `dot` triangleABxN

instance Shape Triangle where
    intersect (Triangle { .. }) (Ray { .. }) =
        --     B     N = AB x AC
        --   -/ \
        --   /   -
        --  /     \
        -- A------>C
        --     q
        -- triangle equation:: pAB + qAC + A , 0<=p, q, p+q<=1
        -- ray equation     :: O + t*D
        -- t*D = pAB + qAC + A - O   -- `dot` N
        -- t = (A - O)*N / DN
        -- p = (t*D - (A - O)) * ACxN / (AB*ACxN)
        -- q = (t*D - (A - O)) * ABxN / (AC*ABxN)
        if t > 0 && (0 <= p && p <= 1) && (0 <= q && q <= 1) && (p + q <= 1)
        then Just t
        else Nothing
      where
        ao = triangleA - rayOrigin
        t = (ao `dot` triangleN) / (rayDirection `dot` triangleN)
        tdo = scale t rayDirection - ao
        p = (tdo `dot` triangleACxN) / triangleABdACxN
        q = (tdo `dot` triangleABxN) / triangleACdABxN


    normalAt t _ = triangleN t

    texture = triangleTexture
