{-# LANGUAGE RecordWildCards #-}

module Graphics.Ray.Types.BoundingBox
     ( BoundingBox
     , fromPoints
     , commonBox
     , disjoint
     , intersects
     ) where

import Data.List(foldl1')

import Data.Vec (Vec, lowerBound, upperBound, cev, unzero)
import Data.Ray (Ray(..))

type BoundingBox = (Vec, Vec)

fromPoints :: [Vec] -> BoundingBox
fromPoints ps = (lowerBound ps, upperBound ps)

commonBox :: [BoundingBox] -> BoundingBox
commonBox = foldl1' common
  where
    common (l1, h1) (l2, h2) = (lowerBound [l1, l2], upperBound [h1, h2])

disjoint :: BoundingBox -> BoundingBox -> Bool
disjoint (l1, h1) (l2, h2) = h1 `less` l2 || h2 `less` l1
  where less a b = let (x, y, z) = cev (b - a)
                   in x > 0 || y > 0 || z > 0

intersects :: Ray -> BoundingBox -> Bool
intersects (Ray { .. }) (l, h) = maximum [lx, ly, lz] < minimum [hx, hy, hz]
  where
    d = unzero rayDirection
    ll = (l - rayOrigin) / d -- I think I do smth wrong with maths =(
    hh = (h - rayOrigin) / d
    lb = lowerBound [ll, hh]
    hb = upperBound [ll, hh]
    (lx, ly, lz) = cev lb
    (hx, hy, hz) = cev hb
