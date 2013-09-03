{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.Ray.Monad
    ( Context(Context)
    , Tracer
    , runTracer

    , getOctree
    , getScene
    , getCamera
    ) where

import Control.Applicative ((<$>))

import Control.Monad.Reader (MonadReader(..), Reader, runReader, asks)

import Graphics.Ray.Octree (Octree)
import Graphics.Ray.Types (Scene(..), Camera(..))

data Context = Context
    { ctxOctree :: !Octree
    , ctxScene  :: !Scene
    }

newtype Tracer a = Tracer { unTracer :: Reader Context a }
    deriving (Monad, Functor)

instance MonadReader Context Tracer where
    ask      = Tracer ask
    local f  = Tracer . local f . unTracer
    reader f = Tracer $ reader f

runTracer :: forall a. Tracer a -> Context -> a
runTracer = runReader . unTracer
{-# INLINE runTracer #-}

getOctree :: Tracer Octree
getOctree = asks ctxOctree
{-# INLINE getOctree #-}

getScene :: Tracer Scene
getScene = asks ctxScene
{-# INLINE getScene #-}

getCamera :: Tracer Camera
getCamera = sceneCamera <$> getScene
{-# INLINE getCamera #-}
