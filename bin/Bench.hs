{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (getNumCapabilities)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import qualified Data.ByteString.Char8 as B
import Control.DeepSeq(deepseq)

import Data.Colour (black, white)
import Data.Vec (Vec, vec, scale)
import Graphics.Ray.Tracer (renderAll)
import Graphics.Ray.Types (Camera, mkCamera,
                           Scene(..),
                           SomeShape(..),
                           PointSource(..), SomeLight(..))
import Text.Obj (parse)

resolution :: (Int, Int)
resolution = (640, 480)

origin :: Vec
origin = vec 0 0 0

up :: Vec
up = vec 0 1 0

camera :: Camera
camera = mkCamera (vec 0 0 100) origin up 80 (40, 30) resolution

mkScene :: [SomeShape] -> Scene
mkScene objs =
    let
        light1 = PointSource (vec 100 0 100) white
        light2 = PointSource (vec (-100) 0 100)  white
    in Scene { sceneCamera = camera
             , sceneShapes = objs
             , sceneLights = [SomeLight light1, SomeLight light2]
             , sceneColour = black
             , sceneLight  = scale 0.1 white
             }

main :: IO ()
main = do
    start <- getCurrentTime
    numCapabilities <- getNumCapabilities
    scene <- mkScene . parse <$> B.getContents
    let !_ = renderAll numCapabilities scene `deepseq` ()
    finish <- getCurrentTime
    putStrLn $ show (diffUTCTime finish start) ++ " seconds"
