module Graphics.UI.SDL.Events.Monad
       ( SDLEventsT
       , MonadSDLEvents
       , withSDLEvents
       ) where

import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Catch

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Class

-- SDL Events monad transformer
newtype SDLEventsT m a = SDLEventsT { runSDLEventsT :: m a }
              deriving ( Functor, Applicative, Monad
                       , MonadFix, MonadIO, MonadThrow
                       , MonadCatch, MonadMask

                       , MonadSDL, MonadSDLVideo
                       , MonadSDLAudio, MonadSDLTimer
                       )

instance MonadTrans SDLEventsT where
  lift = SDLEventsT

instance MonadSDL m => MonadSDLEvents (SDLEventsT m) where

withSDLEvents :: MonadSDL m => SDLEventsT m a -> m a
withSDLEvents = withSubSystem Events . runSDLEventsT
