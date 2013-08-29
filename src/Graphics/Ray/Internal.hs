module Graphics.Ray.Internal
    ( (~==)
    , (~/=)
    ) where

infix 4 ~==, ~/=

(~==) :: (Floating a, Ord a) => a -> a -> Bool
(~==) x y = abs (x - y) < eps where
  eps = 1e-6
{-# INLINE (~==) #-}

(~/=) :: (Floating a, Ord a) => a -> a -> Bool
(~/=) x y = not $ x ~== y
{-# INLINE (~/=) #-}
