{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.SDL.Video (
  SDLVideoT,
  MonadSDLVideo,
  withSDLVideo
  ) where

import Control.Monad
import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Control (MonadTransControl(..),
                                    MonadBaseControl(..),
                                    ComposeSt,
                                    defaultLiftBaseWith,
                                    defaultRestoreM)

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Internal.Class

-- SDL Video monad transformer
newtype SDLVideoT m a = SDLVideoT { runSDLVideoT :: m a }
              deriving (Functor, Applicative, Monad,
                        MonadFix, MonadLogger)

instance MonadTrans SDLVideoT where
  lift = SDLVideoT

instance MonadBase IO m => MonadBase IO (SDLVideoT m) where
  liftBase = lift . liftBase

instance MonadSDLEvents m => MonadSDLVideo (SDLVideoT m) where
instance MonadSDL m => MonadSDL (SDLVideoT m) where
instance MonadSDLAudio m => MonadSDLAudio (SDLVideoT m) where
instance MonadSDLEvents m => MonadSDLEvents (SDLVideoT m) where

instance MonadTransControl SDLVideoT where
     newtype StT SDLVideoT a = StT {unStT :: a}
     liftWith f = SDLVideoT $ f $ runSDLVideoT . liftM StT
     restoreT = SDLVideoT . liftM unStT

instance MonadBaseControl IO m => MonadBaseControl IO (SDLVideoT m) where
  newtype StM (SDLVideoT m) a = StM {unStM :: ComposeSt SDLVideoT m a}
  liftBaseWith = defaultLiftBaseWith StM
  restoreM = defaultRestoreM unStM

withSDLVideo :: (MonadBaseControl IO m, MonadSDLEvents m) => SDLVideoT m a -> m a
withSDLVideo = withSubSystem Video . runSDLVideoT
