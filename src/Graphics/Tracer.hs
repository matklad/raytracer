{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Tracer
    ( trace
    , shade
    , render
    ) where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)

import Data.Colour (Colour, black)
import Data.Ray (Ray, applyRay)
import Data.Vec (Vec)
import Graphics.Camera (applyCamera)
import Graphics.Scene (Scene(..))
import Graphics.Shape (Shape(..), SomeShape, colourAt)

trace :: Ray -> [SomeShape] -> Maybe (SomeShape, Vec)
trace ray shapes = case candidates of
    [] -> Nothing
    _  ->
      let (shape, d) = minimumBy (compare `on` snd) candidates
      in Just (shape, applyRay ray d)
  where
    candidates :: [(SomeShape, Double)]
    candidates =
      mapMaybe (\shape -> (shape, ) <$> shape `intersect` ray) shapes
{-# INLINEABLE trace #-}

shade :: Ray -> Maybe (SomeShape, Vec) -> Colour
shade _ray (Just (shape, v)) = shape `colourAt` v
shade _ray Nothing = black
{-# INLINABLE shade #-}

render :: Scene -> (Int, Int) -> Colour
render (Scene { .. }) p = shade ray $ trace ray sceneShapes where
  ray :: Ray
  ray = applyCamera sceneCamera p
{-# INLINEABLE render #-}
