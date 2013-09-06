{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Graphics.Ray.Octree
    ( Octree
    , mkOctree
    , doIntersect
    , stats
    ) where

import Control.Monad.ST (runST)
import Data.STRef (readSTRef, writeSTRef, newSTRef)
import Control.Monad (forM_, guard)
import Text.Printf (printf)
import Data.List (findIndices, sortBy)
import Data.Maybe (fromJust, isJust)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Vec (scale, vec)
import Data.Ray (Ray(..))
import Graphics.Ray.Types (SomeShape, Shape(..), boundingBox)
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


doIntersect :: Ray -> Octree -> Maybe (SomeShape, Double)
doIntersect ray octree = runST $ do
    cShape <- newSTRef Nothing
    cDist <- newSTRef 1e+6
    let intersectM (Octree { .. }) = do
            case treeChildren of
                [] -> forM_ treeShapes aux
                _  -> do
                    children <- return treeChildren
                    --children <- sortAndCut treeChildren
                    forM_ children go

        sortAndCut trees = do
            current <- readSTRef cDist
            let packed = do
                    o@(Octree { .. }) <- trees
                    let dist = intersects ray treeBox
                        d = fromJust dist
                    guard $ isJust dist && d < current
                    return (d, o)
                sorted = sortBy (\(a, _) (b, _) -> a `compare` b) packed
                unpacked = map snd sorted
            return unpacked

        go o@(Octree { .. }) = do
            let dist = intersects ray treeBox
            current <- readSTRef cDist
            case dist of
                Nothing -> return ()
                (Just d)
                    | d >= current -> return ()
                    | otherwise -> intersectM o

        aux shape = do
            current <- readSTRef cDist
            let md = shape `intersect` ray
            case md of
                Nothing -> return ()
                (Just d) ->  if d < current
                             then do
                                  writeSTRef cDist d
                                  writeSTRef cShape (Just shape)
                             else return ()
    intersectM octree
    shape <- readSTRef cShape
    dist <- readSTRef cDist
    case shape of
        Nothing -> return Nothing
        Just s -> return $ Just (s, dist)

stats :: Octree -> String
stats (Octree { .. }) = printf fmt n_children n_shapes children_stats
  where
    fmt = "%d children, %d shapes\n%s"
    n_children = length treeChildren
    n_shapes = length treeShapes
    children_stats = unlines $ map (" " ++) (lines $ concatMap stats treeChildren)
