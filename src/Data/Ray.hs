{-# LANGUAGE RecordWildCards #-}

module Data.Ray
    ( Ray(..)
    , applyRay
    ) where

import Data.Vec (Vec(..), Normalized, scale)



data Ray = Ray
    { rayOrigin    :: !Vec
    , rayDirection :: !(Normalized Vec)
    } deriving (Eq, Show)


applyRay :: Ray -> Double -> Vec
applyRay (Ray { .. }) d = rayOrigin + d `scale` rayDirection
{-# INLINEABLE applyRay #-}
