module Graphics.Scene
    ( Scene(..)
    ) where

import Graphics.Camera (Camera)
import Graphics.Shape (SomeShape)

data Scene = Scene
    { sceneCamera :: !Camera
    , sceneShapes :: ![SomeShape]
    }
