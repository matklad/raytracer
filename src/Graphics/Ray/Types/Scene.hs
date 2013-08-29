module Graphics.Ray.Types.Scene
    ( Scene(..)
    ) where

import Data.Colour (Colour)
import Graphics.Ray.Types.Camera (Camera)
import Graphics.Ray.Types.Light (SomeLight)
import Graphics.Ray.Types.Shape (SomeShape)

data Scene = Scene
    { sceneCamera :: !Camera
    , sceneShapes :: ![SomeShape]
    , sceneLights :: ![SomeLight]
    , sceneColour :: !Colour
    , sceneLight  :: !Colour
    }
