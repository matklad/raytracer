{-# LANGUAGE RecordWildCards #-}

module Data.Shape
    ( Sphere
    , intersect
    ) where

import Data.Vec
import Data.Ray

data Sphere a = Sphere
    { sphereCenter :: !(Vec a)
    , sphereRadius :: !a
    } deriving Show

-- Just first intersection of sphere and ray or Nothing
intersect :: (Floating a, Ord a) => Sphere a -> Ray a -> Maybe (Vec a)
intersect (Sphere {..}) ray@(Ray {..}) =
    -- sphere equasion:: (center - x)^2 == a^2
    -- ray equasion::    (origin + t*d)
    -- combined and simplified:
    -- t^2 * d^2 -2t * d(center-origin) + (center-origin)^2 - a^2== 0
    -- at^2 + 2bt + c == 0
    if d < 0
    then Nothing
    else case filter (>0) [t1, t2] of
        [] -> Nothing
        xs -> let t = minimum xs in Just $ along ray t

  where
    co = sphereCenter - rayOrigin
    a = rayDirection `dot` rayDirection
    b = rayDirection `dot` co
    c = (co `dot` co) - sphereRadius*sphereRadius
    d = b*b - a*c
    rd = sqrt d
    t1 = (b - rd) / a
    t2 = (b + rd) / a
