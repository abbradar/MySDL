module Graphics.UI.SDL.Audio.Monad
       ( SDLAudioT
       , MonadSDLAudio
       , withSDLAudio
       ) where

import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Catch

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Class

-- SDL Audio monad transformer
newtype SDLAudioT m a = SDLAudioT { runSDLAudioT :: m a }
              deriving ( Functor, Applicative, Monad
                       , MonadFix, MonadIO, MonadThrow
                       , MonadCatch, MonadMask

                       , MonadSDL, MonadSDLVideo
                       , MonadSDLEvents, MonadSDLTimer
                       )

instance MonadTrans SDLAudioT where
  lift = SDLAudioT

instance MonadSDL m => MonadSDLAudio (SDLAudioT m) where

withSDLAudio :: MonadSDL m => SDLAudioT m a -> m a
withSDLAudio = withSubSystem Audio . runSDLAudioT
