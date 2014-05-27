{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.SDL.Events (
  SDLEventsT,
  MonadSDLEvents,
  withSDLEvents
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

-- SDL Events monad transformer
newtype SDLEventsT m a = SDLEventsT { runSDLEventsT :: m a }
              deriving (Functor, Applicative, Monad,
                        MonadFix, MonadLogger)

instance MonadTrans SDLEventsT where
  lift = SDLEventsT

instance MonadBase IO m => MonadBase IO (SDLEventsT m) where
  liftBase = lift . liftBase

instance MonadSDL m => MonadSDLEvents (SDLEventsT m) where
instance MonadSDL m => MonadSDL (SDLEventsT m) where
instance MonadSDLVideo m => MonadSDLVideo (SDLEventsT m) where
instance MonadSDLAudio m => MonadSDLAudio (SDLEventsT m) where

instance MonadTransControl SDLEventsT where
     newtype StT SDLEventsT a = StT {unStT :: a}
     liftWith f = SDLEventsT $ f $ runSDLEventsT . liftM StT
     restoreT = SDLEventsT . liftM unStT

instance MonadBaseControl IO m => MonadBaseControl IO (SDLEventsT m) where
  newtype StM (SDLEventsT m) a = StM {unStM :: ComposeSt SDLEventsT m a}
  liftBaseWith = defaultLiftBaseWith StM
  restoreM = defaultRestoreM unStM

withSDLEvents :: (MonadBaseControl IO m, MonadSDL m) => SDLEventsT m a -> m a
withSDLEvents = withSubSystem Events . runSDLEventsT
