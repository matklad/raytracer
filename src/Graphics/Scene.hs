module Graphics.Scene
    ( Scene(..)
    ) where

import Data.Colour (Colour)
import Graphics.Camera (Camera)
import Graphics.Shape (SomeShape)


data Scene = Scene
    { sceneCamera :: !Camera
    , sceneShapes :: ![SomeShape]
    , sceneColour :: !Colour
    }

