{-# LANGUAGE RecordWildCards #-}

module Graphics.Ray.Octree
    ( Octree
    , mkOctree
    , filterShapes
    , stats
    ) where

import Data.List (elemIndices)
import Text.Printf (printf)

import Data.Vec (scale, vec)
import Data.Ray (Ray(..))
import Graphics.Ray.Types (SomeShape, boundingBox)
import Graphics.Ray.Types.BoundingBox (BoundingBox, commonBox,
                                       disjointWith, intersects)

data Octree = Octree
    { treeBox      :: !BoundingBox
    , treeShapes   :: ![SomeShape]
    , treeChildren :: ![Octree]
    }

mkOctree :: Int -> [SomeShape] -> Octree
mkOctree maxDepth shapes =
    mkOctreeRec maxDepth (commonBox $ map boundingBox shapes) shapes

mkOctreeRec :: Int -> BoundingBox -> [SomeShape] -> Octree
mkOctreeRec depth box@(lBox, hBox) shapes =
    if depth == 1
    then Octree box shapes []
    else Octree box [] children
  where
    diag = scale 0.5 (hBox - lBox)
    x = diag * vec 1 0 0
    y = diag * vec 0 1 0
    z = diag * vec 0 0 1
    zro = vec 0 0 0
    childrenBoxes = do
        sx <- [zro, x]
        sy <- [zro, y]
        sz <- [zro, z]
        let l = lBox + sx + sy + sz
            h = l + diag
        return (l, h)

    partition :: [[SomeShape]] -> [SomeShape] -> [[SomeShape]]
    partition acc [] = acc
    partition c (s:ss) =
        case elemIndices False (map (disjointWith (boundingBox s)) childrenBoxes) of
            []  -> error "impossible happend!"
            xs  ->
                let newC = foldl (\a i -> take i a ++ [s:(a !! i)] ++ drop (i+1) a) c xs
                in partition newC ss

    childrenShapes = partition (replicate 8 []) shapes
    children = [ mkOctreeRec (depth - 1) b s
               | (b, s) <- zip childrenBoxes childrenShapes
               , not (null s)
               ]


filterShapes :: Ray -> Octree-> [SomeShape]
filterShapes ray (Octree { .. }) =
    if intersects ray treeBox
    then treeShapes ++ concatMap (filterShapes ray) treeChildren
    else []

stats :: Octree -> String
stats (Octree { .. }) = printf fmt n_children n_shapes children_stats
  where
    fmt = "%d children, %d shapes\n%s"
    n_children = length treeChildren
    n_shapes = length treeShapes
    children_stats = unlines $ map (" " ++) (lines $ concatMap stats treeChildren)
