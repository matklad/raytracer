module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)

import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString.Char8 as B

import Text.Obj (parse)
import Data.Colour (black, white, toGL)
import Data.Vec (Vec, vec, scale)
import Graphics.Camera (Camera, mkCamera)
import Graphics.Scene (Scene(..))
import Graphics.Shape (SomeShape(..))
import Graphics.Tracer (renderAll)
import Graphics.Light (PointSource(..), SomeLight(..))

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
        light1 = PointSource (vec (100) (0) 100) white
        light2 = PointSource (vec (-100) (0) 100)  white
    in Scene { sceneCamera = camera
             , sceneShapes = objs
             , sceneLights = [SomeLight light1, SomeLight light2]
             , sceneColour = black
             , sceneLight  = scale 0.1 white
             }

display :: Scene -> IO ()
display scene = do
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

createWindowGL :: Scene -> String -> IO ()
createWindowGL scene name = do
    _ <- GLUT.createWindow name
    initGL scene

initGL :: Scene -> IO ()
initGL scene = do
    GLUT.displayCallback $= display scene
    GLUT.reshapeCallback $= Just reshape

main :: IO ()
main = do
    scene <- mkScene . parse <$> B.getContents
    (_, _) <- GLUT.getArgsAndInitialize
    createWindowGL scene "Ray Tracer"
    GLUT.mainLoop
