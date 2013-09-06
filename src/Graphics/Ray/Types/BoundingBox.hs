{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Ray.Types.BoundingBox
     ( BoundingBox
     , fromPoints
     , commonBox
     , disjointWith
     , intersects
     ) where

import Data.List (foldl1')

import Data.Vec (Vec, lowerBound, upperBound, cev, unzero)
import Data.Ray (Ray(..))

type BoundingBox = (Vec, Vec)

fromPoints :: [Vec] -> BoundingBox
fromPoints ps = (lowerBound ps, upperBound ps)
{-# INLINE fromPoints #-}

commonBox :: [BoundingBox] -> BoundingBox
commonBox = foldl1' go where
  go (l1, h1) (l2, h2) = (lowerBound [l1, l2], upperBound [h1, h2])
{-# INLINE commonBox #-}

disjointWith :: BoundingBox -> BoundingBox -> Bool
disjointWith (l1, h1) (l2, h2) = h1 `less` l2 || h2 `less` l1 where
  less a b = let !(x, y, z) = cev (b - a)
             in x > 0 || y > 0 || z > 0
{-# INLINE disjointWith #-}

intersects :: Ray -> BoundingBox -> Maybe Double
intersects (Ray { .. }) (l, h) =
    if lt <= ht then Just lt else Nothing
  where
    d = unzero rayDirection
    ll = (l - rayOrigin) / d -- I think I do smth wrong with maths =(
    hh = (h - rayOrigin) / d
    (lx, ly, lz) = cev $ lowerBound [ll, hh]
    (hx, hy, hz) = cev $ upperBound [ll, hh]
    lt = maximum [lx, ly, lz, 0]
    ht = minimum [hx, hy, hz]
{-# INLINE intersects #-}
