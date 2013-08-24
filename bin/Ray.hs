module Main (main) where

import Control.Monad (forM_)

import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL

import Data.Colour (red, black, blue, white, toGL, green)
import Data.Vec (Vec, vec, scale)
import Graphics.Camera (Camera, mkCamera)
import Graphics.Scene (Scene(..))
import Graphics.Shape (Texture(..), SomeShape(..), sphere, plane)
import Graphics.Tracer (renderAll)
import Graphics.Light (PointSource(..), SomeLight(..))

resolution :: (Int, Int)
resolution = (640, 480)

origin :: Vec
origin = vec 0 0 0

up :: Vec
up = vec 0 0 1
camera :: Camera
camera = mkCamera (vec 0 5 8) origin up 4 (0.4, 0.3) resolution

scene :: Scene
scene =
    let
        p = plane (Solid white) (origin + vec 0 0 0.0) up
        s1 = sphere (Solid white) (origin + vec (-0.1) 0.0 0.1) 0.1
        s2 = sphere (Solid red)   (origin + vec   0.1  0.02 0.1) 0.1

        light1 = PointSource (vec (-3) (-3) 1) (scale 0.3 red)
        light2 = PointSource (vec (3) (3) 1) (scale 0.8 white)
    in Scene { sceneCamera = camera
             , sceneShapes = [SomeShape s1, SomeShape s2, SomeShape p]
             , sceneLights = [SomeLight light1, SomeLight light2]
             , sceneColour = black
             , sceneLight  = scale 0.1 white
             }

display :: IO ()
display = do
    GL.clearColor $= GL.Color4 0 0 0 0
    GL.clear [GL.ColorBuffer]
    GL.renderPrimitive GL.Points $ forM_ (renderAll scene) drawPixel
    GLUT.flush
  where
    drawPixel ((x, y), color) = do
        GL.color $ toGL color
        GL.vertex $ GL.Vertex2 (aux x) (aux y)
    aux :: Int -> GL.GLfloat
    aux = fromRational . toRational


reshape :: GL.Size -> IO ()
reshape _ = do
    GLUT.windowSize $= GL.Size 640 480
    GL.viewport $= (GL.Position 0 0, GL.Size 640 480)
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 640 0 480
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

createWindowGL :: String -> IO ()
createWindowGL name = do
    _ <- GLUT.createWindow name
    initGL

initGL :: IO ()
initGL = do
    GLUT.displayCallback $= display
    GLUT.reshapeCallback $= Just reshape

main :: IO ()
main = do
    (_, _) <- GLUT.getArgsAndInitialize
    createWindowGL "Ray Tracer"
    GLUT.mainLoop
