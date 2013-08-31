module Graphics.Ray.Internal
    ( (~==)
    , (~/=)
    , lInf
    , hInf
    ) where

import Data.Vec (Vec, vec)

infix 4 ~==, ~/=

(~==) :: (Floating a, Ord a) => a -> a -> Bool
(~==) x y = abs (x - y) < eps where
  eps = 1e-6
{-# INLINE (~==) #-}

(~/=) :: (Floating a, Ord a) => a -> a -> Bool
(~/=) x y = not $ x ~== y
{-# INLINE (~/=) #-}

lInf :: Vec
lInf = vec x x x
  where
    x = 10000

hInf :: Vec
hInf = vec x x x
  where
    x = -10000
