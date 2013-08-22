{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Graphics.Shape
    ( Shape(..)
    , SomeShape(..)

    , Texture(..)

    , sphere
    , triangle
    , plane
    ) where

import Control.Exception.Base (assert)

import Data.Colour (Colour)
import Data.Material (Material, simpleMaterial)
import Data.Vec (Vec, Normalized, dot, normalize, scale, cross)
import Data.Ray (Ray(..), applyRay)
import Graphics.Internal ((~/=))


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

    material :: a -> Material

data SomeShape = forall a. Shape a => SomeShape a

instance Shape SomeShape where
    intersect (SomeShape shape) = intersect shape
    normalAt (SomeShape shape)  = normalAt shape
    texture (SomeShape shape)   = texture shape
    material (SomeShape shape)  = material shape

-- * Shapes

data Sphere = Sphere
    { sphereCenter  :: !Vec
    , sphereRadius  :: !Double
    , sphereTexture :: !Texture
    }

sphere :: Texture -> Vec -> Double -> Sphere
sphere sphereTexture sphereCenter sphereRadius = Sphere { .. }
{-# INLINE sphere #-}

instance Shape Sphere where
    intersect (Sphere { .. }) ray@(Ray { .. }) =
        -- Sphere equation:: (C - X)^2 == r^2
        -- ray equation::    (O + t*D)
        -- combined and simplified:
        -- t^2 * D^2 -2t * D(C-O) + (C-O)^2 - r^2== 0
        -- at^2 + 2bt + c == 0
        if d < 0
        then Nothing
        else assert (miss1 + miss2 <= 0.00001) $
             case filter (> 0) [t1, t2] of
                 [] -> Nothing
                 xs -> Just $! seq miss2 (minimum xs)
      where
        co = sphereCenter - rayOrigin
        a  = rayDirection `dot` rayDirection
        b  = rayDirection `dot` co
        rr = sphereRadius*sphereRadius
        c  = (co `dot` co) - rr
        d  = b*b - a*c
        rd = sqrt d
        t1 = (b - rd) / a
        t2 = (b + rd) / a

        v1 = applyRay ray t1 - sphereCenter
        v2 = applyRay ray t2 - sphereCenter
        miss1 = v1 `dot` v1 - rr
        miss2 = v2 `dot` v2 - rr


    normalAt (Sphere { sphereCenter }) x = normalize $ x - sphereCenter
    texture = sphereTexture
    material = const simpleMaterial

data Triangle = Triangle
    { triangleA :: !Vec
    , triangleB :: !Vec
    , triangleC :: !Vec
    , triangleTexture  :: Texture
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
        --
        -- triangle equation:: pAB + qAC + A , 0<=p, q, p+q<=1
        -- ray equation     :: O + t*D
        -- t*D = pAB + qAC + A - O   -- `dot` N
        -- t = (A - O)*N / DN
        -- p = (t*D - (A - O)) * ACxN / (AB*ACxN)
        -- q = (t*D - (A - O)) * ABxN / (AC*ABxN)
        if denom * denom ~/= 0
           && t > 0
           && (0 <= p && p <= 1)
           && (0 <= q && q <= 1)
           && (p + q <= 1)
        then Just t
        else Nothing
      where
        ao = triangleA - rayOrigin
        denom = rayDirection `dot` triangleN
        t = (ao `dot` triangleN) / denom
        tdo = scale t rayDirection - ao
        p = (tdo `dot` triangleACxN) / triangleABdACxN
        q = (tdo `dot` triangleABxN) / triangleACdABxN

    normalAt = const . triangleN
    texture  = triangleTexture
    material = const simpleMaterial


data Plane = Plane
    { planeOffset  :: !Vec
    , planeNormal  :: !(Normalized Vec)
    , planeTexture :: !Texture
    }

plane :: Texture -> Vec -> Normalized Vec -> Plane
plane planeTexture planeOffset planeNormal = Plane { .. }
{-# INLINE plane #-}

instance Shape Plane where
    intersect (Plane { .. }) (Ray { .. }) =
        -- (X - P) * N = 0    Plane
        -- X = O + t * D      Ray
        --
        -- Combined:
        -- (O + t * D - P) * N = 0
        -- t * D * N + (O - P) * N = 0
        -- t = (P - O) * N / (D * N)
        if t >= 0 && q ~/= 0
        then Just t
        else Nothing
      where
        t = p / q
        p = (planeOffset - rayOrigin) `dot` planeNormal
        q = rayDirection `dot` planeNormal

    normalAt = const . planeNormal
    texture  = planeTexture
    material = const simpleMaterial
