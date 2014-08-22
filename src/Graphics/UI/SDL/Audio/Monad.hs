{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.SDL.Audio.Monad
       ( SDLAudioT
       , MonadSDLAudio
       , withSDLAudio
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
import Graphics.UI.SDL.Class

-- SDL Audio monad transformer
newtype SDLAudioT m a = SDLAudioT { runSDLAudioT :: m a }
              deriving (Functor, Applicative, Monad,
                        MonadFix, MonadLogger)

instance MonadTrans SDLAudioT where
  lift = SDLAudioT

deriving instance MonadBase IO m => MonadBase IO (SDLAudioT m)

instance MonadSDL m => MonadSDLAudio (SDLAudioT m) where
deriving instance MonadSDL m => MonadSDL (SDLAudioT m)
deriving instance MonadSDLVideo m => MonadSDLVideo (SDLAudioT m)
deriving instance MonadSDLEvents m => MonadSDLEvents (SDLAudioT m)
deriving instance MonadSDLTimer m => MonadSDLTimer (SDLAudioT m)

instance MonadTransControl SDLAudioT where
     newtype StT SDLAudioT a = StT {unStT :: a}
     liftWith f = SDLAudioT $ f $ runSDLAudioT . liftM StT
     restoreT = SDLAudioT . liftM unStT

instance MonadBaseControl IO m => MonadBaseControl IO (SDLAudioT m) where
  newtype StM (SDLAudioT m) a = StM {unStM :: ComposeSt SDLAudioT m a}
  liftBaseWith = defaultLiftBaseWith StM
  restoreM = defaultRestoreM unStM

withSDLAudio :: (MonadBaseControl IO m, MonadSDL m) => SDLAudioT m a -> m a
withSDLAudio = withSubSystem Audio . runSDLAudioT
