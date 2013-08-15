{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Colour
    ( Colour
    ) where

import Data.Vec (Vec)

-- | Colour representated as float RGB.
newtype Colour = Colour (Vec Double)
  deriving (Show, Eq, Num, Fractional)
