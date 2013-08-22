module Graphics.Scene
    ( Scene(..)
    ) where

import Data.Colour (Colour)
import Graphics.Camera (Camera)
import Graphics.Shape (SomeShape)
import Graphics.Light(SomeLight)

data Scene = Scene
    { sceneCamera :: !Camera
    , sceneShapes :: ![SomeShape]
    , sceneLights :: ![SomeLight]
    , sceneColour :: !Colour
    , sceneLight  :: !Colour
    }
