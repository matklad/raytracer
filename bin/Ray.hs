module Main (main) where

import Control.Monad(forM_)

import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Graphics.Scene(Scene(..))
import Graphics.Camera(mkCamera)
import Graphics.Shape(sphere, triangle, Texture(..), SomeShape(..))
import Graphics.Tracer(renderAll)
import Data.Colour(red, black, green, white, toGL)
import Data.Vec(vec)

scene:: Scene
scene =
    let cLoc  = vec 15 0 5
        cView = vec 0 0 5
        cUp   = vec 0 0 1
        cDist = 5
        width = 640
        heigth = 480
        cSize = (64, 48)
        cRes  = (width, heigth)
        cam   = mkCamera cLoc cView cUp cDist cSize cRes
        s = sphere (Solid white) cView 7
        ta = vec 7 5 0
        tb = vec 7 (-5) 0
        tc = vec 0 0 20
        t = triangle (Solid green) ta tb tc
    in Scene { sceneCamera=cam
             , sceneShapes=[SomeShape s, SomeShape t]
             , sceneColour=black
             , sceneLight=green + red}


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
