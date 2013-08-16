{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Graphics.Tracer
    ( trace
    , render
    ) where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)

import Data.Colour (Colour)
import Data.Ray (Ray, applyRay)
import Data.Vec (Vec)
import Graphics.Camera (applyCamera)
import Graphics.Scene (Scene(..))
import Graphics.Shape (Shape(..), SomeShape, colourAt)

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
shade (Scene { sceneColour }) _ray = maybe sceneColour (uncurry colourAt)
{-# INLINABLE shade #-}

render :: Scene -> (Int, Int) -> Colour
render scene@(Scene { .. }) p = trace scene ray where
  ray :: Ray
  ray = applyCamera sceneCamera p
{-# INLINEABLE render #-}
