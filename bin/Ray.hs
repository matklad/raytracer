module Main (main) where

import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Data.Vec(vec, cev)

import Graphics.Camera(mkCamera, applyCamera)
import Graphics.Shape(Sphere(..), Texture(..), SomeShape(..))
import Graphics.Scene(Scene(..))
import Graphics.Tracer(trace)
import Data.Colour(red, black, getRGB)
import Control.Monad(forM_)


display :: IO ()
display = do
    GL.clearColor $= GL.Color4 0 0 0 0
    GL.clear [GL.ColorBuffer]
    GL.renderPrimitive GL.Lines $ do
      GL.color $ GL.Color3 (1.0 :: GL.GLfloat) 0 0
      GL.vertex $ GL.Vertex2 (0 :: GL.GLfloat) 0
      GL.vertex $ GL.Vertex2 (640 :: GL.GLfloat) 480
      GL.vertex $ GL.Vertex2 (0 :: GL.GLfloat) 480
      GL.vertex $ GL.Vertex2 (640 :: GL.GLfloat) 0
    GLUT.flush

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

--main :: IO ()
--main = do
--    (_, _) <- GLUT.getArgsAndInitialize
--    createWindowGL "Ray Tracer"
--    GLUT.mainLoop

main :: IO ()
main = do
    let cLoc  = vec 10 0 5
        cView = vec 0 0 5
        cUp   = vec 0 0 1
        cDist = 5
        cSize = (10, 10)
        x = 100
        y = 100
        cRes  = (x, y)
        cam   = mkCamera cLoc cView cUp cDist cSize cRes
        sphere = Sphere cView 3 (Solid red)
        scene  = Scene { sceneCamera=cam
                       , sceneShapes=[SomeShape sphere]
                       , sceneColour=black}

        colours = [[let
                        ray = applyCamera cam (i, j)
                        c = trace scene ray
                        rgb = cev $ getRGB c
                   in
                    if rgb == (0.0, 0.0, 0.0)
                    then '.'
                    else 'X'
                   | i <- [1..x]]
                  | j <- [1..y]]
    forM_ colours putStrLn
