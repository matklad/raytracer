{-# LANGUAGE OverloadedStrings #-}

module Graphics.Ray.Ppm (writePpm) where

import System.IO (Handle)
import Control.Monad (forM_)

import Data.Binary.Put (Put, runPut, putByteString, putWord8)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import Data.Array (Array, bounds, elems, (!))
import Data.Word ()

import Data.Colour (Colour, toRGB8)



writePpm :: Handle -> Array (Int, Int) Colour -> IO ()
writePpm h img = B.hPut h $ runPut (serializeImg img)


serializeImg :: Array (Int, Int) Colour -> Put
serializeImg img = do
  putByteString "P6\n"
  putByteString (C.pack . show $ width + 1)
  putByteString " "
  putByteString (C.pack . show $ height + 1)
  putByteString "\n255\n"
  forM_ [(x, y)| y <- [height, height - 1..0], x <- [0..width]] putPixel
  where
    (_, (width, height)) = bounds img
    putPixel (x, y) =
      let c = img ! (x, y)
          (r, g, b) = toRGB8 c in
       do
         putWord8 r
         putWord8 g
         putWord8 b
