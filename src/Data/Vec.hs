module Data.Vec
    ( Vec
    , vec

    , I.Normalized

    , I.length
    , I.scale
    , I.normalize
    , I.dot
    , I.cross
    ) where

import qualified Data.Vec.Internal as I

type Vec = I.Vec Double

vec :: Double -> Double -> Double -> Vec
vec = I.Vec
{-# INLINE vec #-}
