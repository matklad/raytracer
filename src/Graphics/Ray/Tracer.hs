{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Graphics.Ray.Tracer
    ( trace
    , render
    , renderAll
    ) where

import Control.Monad (filterM)
import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (minimumBy)

import Data.Array (Array, listArray)
import Control.Parallel.Strategies (using, rdeepseq, parBuffer)

import Data.Colour (Colour)
import Data.Ray (Ray(..), applyRay)
import Data.Vec (Vec, scale, dot)
import Graphics.Ray.Monad (Context(..), Tracer, getOctree, getScene, runTracer)
import Graphics.Ray.Types (Scene(..),
                           Camera(..), applyCamera,
                           LightSource(..), Light(..),
                           Material(..),
                           Shape(..), SomeShape(..), colourAt)
import Graphics.Ray.Octree (foldForRay, mkOctree)

findIntersection :: Ray -> Tracer (Maybe (SomeShape, Double))
findIntersection ray = do
    octree <- getOctree
    return $ foldForRay ray f Nothing octree
  where
    f Nothing shape = aux shape
    f (Just c) shape = case aux shape of
        Nothing -> Just c
        Just x  ->  Just $ minimumBy (compare `on` snd) [c, x]
    aux shape = (shape, ) <$> shape `intersect` ray
{-# INLINE findIntersection #-}

trace ::  Ray -> Tracer Colour
trace ray = do
    intersection <- findIntersection ray
    let shapePoint = do
            (shape, d) <- intersection
            return (shape, applyRay ray d)
    shade ray shapePoint
{-# INLINEABLE trace #-}

shade :: Ray -> Maybe (SomeShape, Vec) -> Tracer Colour
shade _view Nothing              = sceneColour <$> getScene
shade view (Just (shape, point)) = do
    lights <- sceneLights <$> getScene
    ambientLight <- sceneLight <$> getScene
    visibleLights <- filterM isVisible $ map (\l -> shed l point n) lights
    let ambient    = baseColour * materialAmbient * ambientLight
        diffuse    = sum $ map computeDiffuse visibleLights
        specular   = sum $ map computeSpecular visibleLights
    return $ ambient + diffuse + specular
  where
    baseColour = colourAt shape point
    n = normalAt shape point
    (Material { .. }) = material shape
    viewDirection = rayDirection view

    isVisible :: Light -> Tracer Bool
    isVisible (Light { .. }) = do
        let microShift = scale 0.00001 lightDirection
            ray = Ray (point + microShift) lightDirection
        intersection <- findIntersection ray
        return $ case intersection of
            Nothing -> True
            Just (_, d) -> d > lightDistance

    computeDiffuse :: Light -> Colour
    computeDiffuse (Light { .. }) =
        scale lightPower (baseColour * lightColour * materialDiffuse)

    computeSpecular :: Light -> Colour
    computeSpecular (Light { .. }) =
        let rview = reflect viewDirection n
            k = max 0 (rview `dot` lightDirection) ** materialPhong
        in scale k (lightColour * materialSpecular)
{-# INLINABLE shade #-}

reflect :: Vec -> Vec -> Vec
reflect v n = r
  where
    nproj = scale (v `dot` (-n)) n
    d = v + nproj
    r = nproj + d

render :: (Int, Int) -> Tracer Colour
render pixel = do
    camera <- sceneCamera <$> getScene
    let ray = applyCamera camera pixel
    trace ray
{-# INLINEABLE render #-}

renderAll :: Int -> Scene -> Array (Int, Int) Colour
renderAll numCapabilities scene@(Scene { .. }) = ret
  where
    (w, h)    = camScreenResolution sceneCamera
    (mx, my)  = (pred w, pred h)
    chunkSize = w * h `div` numCapabilities
    strat     = parBuffer chunkSize rdeepseq
    octree    = mkOctree 4 sceneShapes
    ctx       = Context octree scene
    pixels    = [(x, y)| x <- [0..mx], y <- [0..my]]
    colours   = [render p `runTracer` ctx | p <- pixels] `using` strat
    ret      = listArray ((0, 0), (mx, my)) colours
