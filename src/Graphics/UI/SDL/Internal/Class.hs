module Graphics.UI.SDL.Internal.Class where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Catch

class (Applicative m, MonadIO m, MonadMask m) => MonadSDL m where
class MonadSDL m => MonadSDLAudio m where
class MonadSDL m => MonadSDLEvents m where
class MonadSDL m => MonadSDLTimer m where
class (MonadSDLEvents m, MonadSDLTimer m) => MonadSDLVideo m where
