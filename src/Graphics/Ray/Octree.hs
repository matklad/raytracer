{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Ray.Octree
    ( Octree
    , mkOctree
    , foldForRay
    , stats
    ) where

import Data.List (findIndices, foldl')
import Text.Printf (printf)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

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
    else Octree box []
         [ mkOctreeRec (depth - 1) b s
         | (b, s) <- zip childrenBoxes childrenShapes
         , not (null s)
         ]
  where
    diag = scale 0.5 (hBox - lBox)
    x = diag * vec 1 0 0
    y = diag * vec 0 1 0
    z = diag * vec 0 0 1
    zro = vec 0 0 0

    childrenShapes = partition (IntMap.fromAscList $ zip [0..7] (repeat [])) shapes
    childrenBoxes  = do
        sx <- [zro, x]
        sy <- [zro, y]
        sz <- [zro, z]
        let l = lBox + sx + sy + sz
            h = l + diag
        return (l, h)

    partition :: IntMap [SomeShape] -> [SomeShape] -> [[SomeShape]]
    partition !acc [] =
        -- Note(superbobry): thanks to the initializer, we _always_ have
        -- an 'IntMap' of size 8.
        IntMap.elems acc
    partition !acc (s:ss) =
        case findIndices (not . (boundingBox s `disjointWith`)) childrenBoxes of
            [] -> error "partition: the impossible happened!"
            is -> partition (foldr (IntMap.adjust (s:)) acc is) ss

foldForRay :: Ray -> (a -> SomeShape -> a) -> a -> Octree-> a
foldForRay ray f start (Octree { .. }) =
    case (intersects ray treeBox, treeChildren) of
        (False, _) -> start
        (True, []) -> foldl' f start treeShapes
        (True, _ ) -> foldl' (foldForRay ray f) start treeChildren


stats :: Octree -> String
stats (Octree { .. }) = printf fmt n_children n_shapes children_stats
  where
    fmt = "%d children, %d shapes\n%s"
    n_children = length treeChildren
    n_shapes = length treeShapes
    children_stats = unlines $ map (" " ++) (lines $ concatMap stats treeChildren)
