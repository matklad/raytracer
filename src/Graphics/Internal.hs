module Graphics.Internal
    ( (~==)
    , (~/=)
    ) where

infix 4 ~==, ~/=

(~==) :: (Floating a, Ord a) => a -> a -> Bool
(~==) x y = x - y < eps where
  eps = 1e-6
{-# INLINE (~==) #-}

(~/=) :: (Floating a, Ord a) => a -> a -> Bool
(~/=) x y = not $ x ~== y
{-# INLINE (~/=) #-}
