module Graphics.Types.Scene
    ( Scene(..)
    ) where

import Data.Colour (Colour)
import Graphics.Types.Camera (Camera)
import Graphics.Types.Shape (SomeShape)
import Graphics.Types.Light(SomeLight)

data Scene = Scene
    { sceneCamera :: !Camera
    , sceneShapes :: ![SomeShape]
    , sceneLights :: ![SomeLight]
    , sceneColour :: !Colour
    , sceneLight  :: !Colour
    }
