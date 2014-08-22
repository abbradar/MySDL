{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.SDL.Timer.Monad
       ( SDLTimerT
       , MonadSDLTimer
       , withSDLTimer
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

-- SDL Timer monad transformer
newtype SDLTimerT m a = SDLTimerT { runSDLTimerT :: m a }
              deriving (Functor, Applicative, Monad,
                        MonadFix, MonadLogger)

instance MonadTrans SDLTimerT where
  lift = SDLTimerT

deriving instance MonadBase IO m => MonadBase IO (SDLTimerT m)

instance MonadSDL m => MonadSDLTimer (SDLTimerT m) where
deriving instance MonadSDL m => MonadSDL (SDLTimerT m)
deriving instance MonadSDLVideo m => MonadSDLVideo (SDLTimerT m)
deriving instance MonadSDLAudio m => MonadSDLAudio (SDLTimerT m)
deriving instance MonadSDLEvents m => MonadSDLEvents (SDLTimerT m)

instance MonadTransControl SDLTimerT where
     newtype StT SDLTimerT a = StT {unStT :: a}
     liftWith f = SDLTimerT $ f $ runSDLTimerT . liftM StT
     restoreT = SDLTimerT . liftM unStT

instance MonadBaseControl IO m => MonadBaseControl IO (SDLTimerT m) where
  newtype StM (SDLTimerT m) a = StM {unStM :: ComposeSt SDLTimerT m a}
  liftBaseWith = defaultLiftBaseWith StM
  restoreM = defaultRestoreM unStM

withSDLTimer :: (MonadBaseControl IO m, MonadSDL m) => SDLTimerT m a -> m a
withSDLTimer = withSubSystem Timer . runSDLTimerT
