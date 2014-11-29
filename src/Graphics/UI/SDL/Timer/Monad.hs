module Graphics.UI.SDL.Timer.Monad
       ( SDLTimerT
       , MonadSDLTimer
       , withSDLTimer
       ) where

import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Control (MonadBaseControl(..))

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Class

-- SDL Timer monad transformer
newtype SDLTimerT m a = SDLTimerT { runSDLTimerT :: m a }
              deriving (Functor, Applicative, Monad,
                        MonadFix, MonadLogger)

instance MonadTrans SDLTimerT where
  lift = SDLTimerT

deriving instance MonadBase IO m => MonadBase IO (SDLTimerT m)
deriving instance MonadIO m => MonadIO (SDLTimerT m)

instance MonadSDL m => MonadSDLTimer (SDLTimerT m) where
deriving instance MonadSDL m => MonadSDL (SDLTimerT m)
deriving instance MonadSDLVideo m => MonadSDLVideo (SDLTimerT m)
deriving instance MonadSDLAudio m => MonadSDLAudio (SDLTimerT m)
deriving instance MonadSDLEvents m => MonadSDLEvents (SDLTimerT m)

instance (MonadBaseControl IO m, MonadSDL m) => MonadBaseControl IO (SDLTimerT m) where
  newtype StM (SDLTimerT m) a = StM {unStM :: StM m a}
  liftBaseWith = liftBaseThreaded SDLTimerT runSDLTimerT withSDLTimer StM
  restoreM = SDLTimerT . restoreM . unStM

withSDLTimer :: (MonadBaseControl IO m, MonadSDL m) => SDLTimerT m a -> m a
withSDLTimer = withSubSystem Timer . runSDLTimerT
