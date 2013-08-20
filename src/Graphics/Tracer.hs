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

import Data.Colour (Colour, black)
import Data.Material (Material, materialAmbient)
import Data.Ray (Ray, applyRay)
import Data.Vec (Vec)
import Graphics.Camera (applyCamera, camScreenResolution)
import Graphics.Scene (Scene(..))
import Graphics.Shape (Shape(..), SomeShape(..), colourAt)

intersections :: [SomeShape] -> Ray -> [(SomeShape, Double)]
intersections shapes ray =
    mapMaybe (\shape -> (shape, ) <$> shape `intersect` ray) shapes
{-# INLINE intersections #-}

trace :: Scene -> Ray -> Colour
trace scene@(Scene { .. }) ray = shade scene ray $ do
    (shape, d) <- closest
    return (shape, applyRay ray d)
  where
    closest :: Maybe (SomeShape, Double)
    closest = case intersections sceneShapes ray of
        []          -> Nothing
        candidates  -> Just $ minimumBy (compare `on` snd) candidates
{-# INLINEABLE trace #-}

shade :: Scene -> Ray -> Maybe (SomeShape, Vec) -> Colour
shade (Scene { .. }) _ Nothing = sceneColour
shade (Scene { .. }) _view (Just (shape, point)) =
    ambient + diffuse + specular
  where
    baseColour = colourAt shape point
    ambient    = baseColour * (materialAmbient . material $ shape) * sceneLight
    diffuse    = black
    specular   = black
{-# INLINABLE shade #-}

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
     [((x, y), render scene (x, y))| y <- [0..w],x <- [0..h]]
