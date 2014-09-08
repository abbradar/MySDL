{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.SDL.Video.Monad
       ( SDLVideoT
       , MonadSDLVideo
       , withSDLVideo
       ) where

import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Control (MonadBaseControl(..))

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Class

-- SDL Video monad transformer
newtype SDLVideoT m a = SDLVideoT { runSDLVideoT :: m a }
              deriving (Functor, Applicative, Monad,
                        MonadFix, MonadLogger)

instance MonadTrans SDLVideoT where
  lift = SDLVideoT

deriving instance MonadBase IO m => MonadBase IO (SDLVideoT m)

instance (MonadSDLTimer m, MonadSDLEvents m) => MonadSDLVideo (SDLVideoT m) where
deriving instance MonadSDL m => MonadSDL (SDLVideoT m)
deriving instance MonadSDLAudio m => MonadSDLAudio (SDLVideoT m)
deriving instance MonadSDLEvents m => MonadSDLEvents (SDLVideoT m)
deriving instance MonadSDLTimer m => MonadSDLTimer (SDLVideoT m)

instance (MonadBaseControl IO m, MonadSDLEvents m, MonadSDLTimer m) => MonadBaseControl IO (SDLVideoT m) where
  newtype StM (SDLVideoT m) a = StM {unStM :: StM m a}
  liftBaseWith = liftBaseThreaded SDLVideoT runSDLVideoT withSDLVideo StM
  restoreM = SDLVideoT . restoreM . unStM

withSDLVideo :: (MonadBaseControl IO m, MonadSDLEvents m, MonadSDLTimer m) => SDLVideoT m a -> m a
withSDLVideo = withSubSystem Video . runSDLVideoT
