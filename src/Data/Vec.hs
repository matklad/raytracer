module Data.Vec
    ( Vec
    , vec

    , Internal.Normalized

    , Internal.length
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
