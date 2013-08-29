{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Graphics.Tracer
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
import Graphics.Types.Material (Material(..))
import Graphics.Types.Camera (applyCamera, camScreenResolution)
import Graphics.Types.Scene (Scene(..))
import Graphics.Types.Shape (Shape(..), SomeShape(..), colourAt)
import Graphics.Types.Light (LightSource(..), Light(..))


findIntersection :: [SomeShape] -> Ray -> Maybe (SomeShape, Double)
findIntersection shapes ray = case all_intersections of
    [] -> Nothing
    candidates  -> Just $ minimumBy (compare `on` snd) candidates
  where
    aux shape = (shape, ) <$> shape `intersect` ray
    all_intersections = mapMaybe aux shapes
{-# INLINE findIntersection #-}

trace :: Scene -> Ray -> Colour
trace scene@(Scene { .. }) ray = shade scene ray $ do
    (shape, d) <- findIntersection sceneShapes ray
    return (shape, applyRay ray d)

{-# INLINEABLE trace #-}

shade :: Scene -> Ray -> Maybe (SomeShape, Vec) -> Colour
shade (Scene { .. }) _ Nothing = sceneColour
shade (Scene { .. }) view (Just (shape, point)) =
    ambient + diffuse + specular
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
renderAll scene =
    let camera = sceneCamera scene
        (w, h) = camScreenResolution camera
    in
     [((x, y), render scene (x, y))| x <- [0..w], y <- [0..h]]
