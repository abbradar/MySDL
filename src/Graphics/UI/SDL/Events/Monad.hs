{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.UI.SDL.Events.Monad
       ( SDLEventsT
       , MonadSDLEvents
       , withSDLEvents
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

-- SDL Events monad transformer
newtype SDLEventsT m a = SDLEventsT { runSDLEventsT :: m a }
              deriving (Functor, Applicative, Monad,
                        MonadFix, MonadLogger)

instance MonadTrans SDLEventsT where
  lift = SDLEventsT

deriving instance MonadBase IO m => MonadBase IO (SDLEventsT m)

instance MonadSDL m => MonadSDLEvents (SDLEventsT m) where
deriving instance MonadSDL m => MonadSDL (SDLEventsT m)
deriving instance MonadSDLVideo m => MonadSDLVideo (SDLEventsT m)
deriving instance MonadSDLAudio m => MonadSDLAudio (SDLEventsT m)
deriving instance MonadSDLTimer m => MonadSDLTimer (SDLEventsT m)

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
