{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Graphics.Tracer
    ( trace
    , shade
    ) where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)

import Data.Colour (Colour)
import Data.Ray (Ray, applyRay)
import Data.Vec (Vec)
import Graphics.Shape (Shape(..), SomeShape)

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

shade :: Ray -> (SomeShape, Vec) -> Colour
shade _ray (shape, v) = shape `colourAt` v
{-# INLINABLE shade #-}
