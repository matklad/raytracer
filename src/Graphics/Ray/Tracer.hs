{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Graphics.Ray.Tracer
    ( trace
    , render
    , renderAll
    ) where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)

import Data.Array (Array, listArray)
import Control.Parallel.Strategies (using, rdeepseq, parBuffer)

import Data.Colour (Colour)
import Data.Ray (Ray(..), applyRay)
import Data.Vec (Vec, scale, dot)
import Graphics.Ray.Monad (Tracer, getOctree, getScene, getCamera)
import Graphics.Ray.Types (Scene(..),
                           Camera(..), applyCamera,
                           LightSource(..), Light(..),
                           Material(..),
                           Shape(..), SomeShape(..), colourAt)
import Graphics.Ray.Octree (Octree, filterShapes)

findIntersection :: [SomeShape] -> Ray -> Maybe (SomeShape, Double)
findIntersection shapes ray = case mapMaybe go shapes of
    []          -> Nothing
    candidates  -> Just $ minimumBy (compare `on` snd) candidates
  where
    go shape = (shape, ) <$> shape `intersect` ray
{-# INLINE findIntersection #-}

trace :: Octree -> Scene -> Ray -> Colour
trace octree scene@(Scene { .. }) ray =
    let shapes = filterShapes ray octree
    in shade octree scene ray $ do
        (shape, d) <- findIntersection shapes ray
        return (shape, applyRay ray d)
{-# INLINEABLE trace #-}

shade :: Octree -> Scene -> Ray -> Maybe (SomeShape, Vec) -> Colour
shade _octree (Scene { .. }) _view Nothing              = sceneColour
shade octree  (Scene { .. }) view (Just (shape, point)) = ambient + diffuse + specular
  where
    baseColour = colourAt shape point
    n = normalAt shape point
    (Material { .. }) = material shape
    viewDirection = rayDirection view

    isVisible :: Light -> Bool
    isVisible (Light { .. }) =
        case intersection of
            Nothing -> True
            Just (_, d) -> d > lightDistance
      where
        microShift = scale 0.00001 lightDirection
        ray = Ray (point + microShift) lightDirection
        intersection = findIntersection (filterShapes ray octree) ray

    computeDiffuse :: Light -> Colour
    computeDiffuse (Light { .. }) =
        scale lightPower (baseColour * lightColour * materialDiffuse)

    computeSpecular :: Light -> Colour
    computeSpecular (Light { .. }) =
        let rview = reflect viewDirection n
            k = max 0 (rview `dot` lightDirection) ** materialPhong
        in scale k (lightColour * materialSpecular)

    visibleLights = filter isVisible $ map (\l -> shed l point n) sceneLights

    ambient    = baseColour * materialAmbient * sceneLight
    diffuse    = sum $ map computeDiffuse visibleLights
    specular   = sum $ map computeSpecular visibleLights
{-# INLINABLE shade #-}

reflect :: Vec -> Vec -> Vec
reflect v n = r
  where
    nproj = scale (v `dot` (-n)) n
    d = v + nproj
    r = nproj + d

render :: (Int, Int) -> Tracer Colour
render pixel = do
    octree <- getOctree
    scene@(Scene { sceneCamera }) <- getScene
    let ray = applyCamera sceneCamera pixel
    return $! trace octree scene ray
{-# INLINEABLE render #-}

renderAll :: Int -> Tracer (Array (Int, Int) Colour)
renderAll numCapabilities = do
    (Camera { camScreenResolution = (w, h) }) <- getCamera
    pixels <- mapM render [(x, y) | x <- [0..pred w], y <- [0..pred h]]
    let chunkSize = w * h `div` numCapabilities
    return . listArray ((0, 0), (pred w, pred h)) $
        (pixels `using` parBuffer chunkSize rdeepseq)
