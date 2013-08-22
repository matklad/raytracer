{-# LANGUAGE RecordWildCards #-}

module Data.Vec
    ( Vec
    , vec
    , cev

    , Internal.Normalized

    , Internal.norm
    , Internal.scale
    , Internal.normalize
    , Internal.dot
    , Internal.cross
    ) where

import qualified Data.Vec.Internal as Internal

type Vec = Internal.Vec Double

vec :: Double -> Double -> Double -> Vec
vec = Internal.Vec
{-# INLINE vec #-}


cev :: Vec -> (Double, Double, Double)
cev (Internal.Vec { .. }) = (vecX, vecY, vecZ)
