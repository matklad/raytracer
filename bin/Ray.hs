{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, void)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Data.Array (Array)
import Graphics.Rendering.OpenGL (($=))
import qualified Data.Array as Array
import qualified Data.ByteString.Char8 as B
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT

import Data.Colour (Colour, black, white, toGL)
import Data.Vec (Vec, vec, scale)
import Graphics.Ray.Monad (Context(..), runTracer)
import Graphics.Ray.Octree (mkOctree)
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
camera = mkCamera (vec 50 50 80) origin up 80 (40, 30) resolution

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

display :: Array (Int, Int) Colour -> IO ()
display pixels = do
    start <- getCurrentTime
    GL.clearColor $= GL.Color4 0 0 0 0
    GL.clear [GL.ColorBuffer]
    GL.renderPrimitive GL.Points $ forM_ (Array.assocs pixels) drawPixel
    GLUT.flush
    GLUT.swapBuffers
    finish <- getCurrentTime
    print $ show (diffUTCTime finish start) ++ " for frame"
  where
    drawPixel ((x, y), color) = do
        GL.color $ toGL color
        GL.vertex $ GL.Vertex2
            (fromIntegral x :: GL.GLint) (fromIntegral y :: GL.GLint)


reshape :: GL.Size -> IO ()
reshape _size = do
    GLUT.windowSize $= GL.Size 640 480
    GL.viewport $= (GL.Position 0 0, GL.Size 640 480)
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 640 0 480
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity


main :: IO ()
main = do
    scene@(Scene { sceneShapes }) <- mkScene . parse <$> B.getContents
    let octree  = mkOctree 4 sceneShapes
        ctx     = Context octree scene
        !pixels = renderAll `runTracer` ctx
    (_, _) <- GLUT.getArgsAndInitialize
    GLUT.initialDisplayMode $= [GLUT.DoubleBuffered]
    void $ GLUT.createWindow "Ray"
    GLUT.displayCallback $= display pixels
    GLUT.reshapeCallback $= Just reshape
    GLUT.mainLoop
