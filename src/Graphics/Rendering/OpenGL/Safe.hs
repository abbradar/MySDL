{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.OpenGL.Safe
       ( OpenGLException(..)
       , OpenGLT
       , checkErrors
       , withSafeGL
       ) where

import Control.Monad
import Data.Typeable (Typeable)
import Control.Exception.Lifted (Exception, throwIO)
import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Graphics.Rendering.OpenGL.GL.StateVar (get)
import Graphics.Rendering.OpenGL.GLU.Errors (Error, errors)

import Graphics.UI.SDL.Class

newtype OpenGLException = OpenGLException [Error]
                          deriving (Eq, Show, Typeable)

instance Exception OpenGLException

newtype OpenGLT m a = OpenGLT { runOpenGLT :: m a }
              deriving (Functor, Applicative)

instance MonadTrans OpenGLT where
  lift = OpenGLT

deriving instance MonadBase IO m => MonadBase IO (OpenGLT m)

checkErrors :: MonadBase IO m => m ()
checkErrors = liftBase $ do
  err <- liftBase $ get errors
  unless (null err) $ throwIO $ OpenGLException err

instance MonadBase IO m => Monad (OpenGLT m) where
  a >>= b = OpenGLT $ do
    r <- runOpenGLT a
    checkErrors
    runOpenGLT $ b r
  return = lift . return
  fail = lift . fail

deriving instance (Monad (OpenGLT m), MonadLogger m) => MonadLogger (OpenGLT m)
deriving instance (Monad (OpenGLT m), MonadFix m) => MonadFix (OpenGLT m)

instance MonadSDL m => MonadSDL (OpenGLT m) where
instance MonadSDLAudio m => MonadSDLAudio (OpenGLT m) where
instance MonadSDLEvents m => MonadSDLEvents (OpenGLT m) where
instance MonadSDLTimer m => MonadSDLTimer (OpenGLT m) where
instance MonadSDLVideo m => MonadSDLVideo (OpenGLT m) where

-- We cannot make OpenGLT an instance of MonadTransControl, because OpenGLT is only a
-- monad within some context, so it's not a valid transformer.

instance (Monad (OpenGLT m), MonadBaseControl IO m) => MonadBaseControl IO (OpenGLT m) where
  newtype StM (OpenGLT m) a = StM {unStM :: StM m a}
  liftBaseWith f = OpenGLT $ liftBaseWith $ \x -> f $ liftM StM . x . runOpenGLT
  restoreM = OpenGLT . restoreM . unStM

withSafeGL :: MonadBase IO m => OpenGLT m a -> m a
withSafeGL = runOpenGLT
