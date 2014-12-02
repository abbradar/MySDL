module Graphics.UI.SDL
       ( SDLError(..)
       , module Monad
       , module Types
       , module Audio
       , module Timer
       , module Events
       , module Video
       , module Version
       , withSDLAllVideo
       , withSDLAll
       ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Catch

import Graphics.UI.SDL.Internal.Prim

import Graphics.UI.SDL.Monad as Monad
import Graphics.UI.SDL.Types as Types
import Graphics.UI.SDL.Audio as Audio
import Graphics.UI.SDL.Timer as Timer
import Graphics.UI.SDL.Events as Events
import Graphics.UI.SDL.Video as Video
import Graphics.UI.SDL.Version as Version

-- | Initialize SDL with all video-related subsystems.
withSDLAllVideo :: (Applicative m, MonadIO m, MonadMask m) =>
                   SDLVideoT (SDLTimerT (SDLEventsT (SDLT m))) a -> m a
withSDLAllVideo = withSDL . withSDLEvents . withSDLTimer . withSDLVideo

-- | Initialize SDL with all subsystems.
withSDLAll :: (Applicative m, MonadIO m, MonadMask m) =>
              SDLAudioT (SDLVideoT (SDLTimerT (SDLEventsT (SDLT m)))) a -> m a
withSDLAll = withSDLAllVideo . withSDLAudio
