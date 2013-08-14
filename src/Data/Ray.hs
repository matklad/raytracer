{-# LANGUAGE RecordWildCards #-}

module Data.Ray
    ( Ray(..)
    , along
    ) where

import Data.Vec (Vec(..), Normalized)
import qualified Data.Vec as Vec

data Ray a = Ray { rayOrigin    :: !(Vec a)
                 , rayDirection :: !(Normalized (Vec a))
                 }
    deriving (Eq, Show)

along :: Num a => Ray a -> a -> Vec a
along (Ray { .. }) d = rayOrigin + d `Vec.scale` rayDirection
{-# SPECIALIZE INLINE along :: Ray Double -> Double -> Vec Double #-}
