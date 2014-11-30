module Graphics.UI.SDL.Timer.Monad
       ( SDLTimerT
       , MonadSDLTimer
       , withSDLTimer
       ) where

import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Catch

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Class

-- SDL Timer monad transformer
newtype SDLTimerT m a = SDLTimerT { runSDLTimerT :: m a }
              deriving ( Functor, Applicative, Monad
                       , MonadFix, MonadIO, MonadThrow
                       , MonadCatch, MonadMask

                       , MonadSDL, MonadSDLVideo
                       , MonadSDLAudio, MonadSDLEvents
                       )

instance MonadTrans SDLTimerT where
  lift = SDLTimerT

instance MonadSDL m => MonadSDLTimer (SDLTimerT m) where

withSDLTimer :: MonadSDL m => SDLTimerT m a -> m a
withSDLTimer = withSubSystem Timer . runSDLTimerT
