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

import Data.Colour (Colour)
import Data.Ray (Ray(..), applyRay)
import Data.Vec (Vec, scale, dot)
import Graphics.Ray.Types (applyCamera, camScreenResolution,
                           LightSource(..), Light(..),
                           Material(..),
                           Scene(..),
                           Shape(..), SomeShape(..), colourAt)


findIntersection :: [SomeShape] -> Ray -> Maybe (SomeShape, Double)
findIntersection shapes ray = case mapMaybe go shapes of
    []          -> Nothing
    candidates  -> Just $ minimumBy (compare `on` snd) candidates
  where
    go shape = (shape, ) <$> shape `intersect` ray
{-# INLINE findIntersection #-}

trace :: Scene -> Ray -> Colour
trace scene@(Scene { .. }) ray = shade scene ray $ do
    (shape, d) <- findIntersection sceneShapes ray
    return (shape, applyRay ray d)
{-# INLINEABLE trace #-}

shade :: Scene -> Ray -> Maybe (SomeShape, Vec) -> Colour
shade (Scene { .. }) _view Nothing              = sceneColour
shade (Scene { .. }) view (Just (shape, point)) = ambient + diffuse + specular
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
        intersection = findIntersection sceneShapes ray

    computeDiffuse :: Light -> Colour
    computeDiffuse (Light { .. }) =
        scale lightPower (baseColour * lightColour * materialDiffuse)

    computeSpecular :: Light -> Colour
    computeSpecular (Light { .. }) =
        let rview = reflect viewDirection n
            k = (max 0 $ rview `dot` lightDirection) ** materialPhong
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

render :: Scene -> (Int, Int) -> Colour
render scene@(Scene { .. }) p = trace scene ray where
  ray :: Ray
  ray = applyCamera sceneCamera p
{-# INLINEABLE render #-}


renderAll :: Scene -> [((Int, Int), Colour)]
renderAll scene@(Scene { sceneCamera }) = do
    x <- [0..w]
    y <- [0..h]
    return ((x, y), render scene (x, y))
  where
    !(w, h) = camScreenResolution sceneCamera
