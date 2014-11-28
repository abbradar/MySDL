module Graphics.UI.SDL.Internal.Class where

import Control.Monad.Base (MonadBase)

class MonadBase IO m => MonadSDL m where
class MonadSDL m => MonadSDLAudio m where
class MonadSDL m => MonadSDLEvents m where
class MonadSDL m => MonadSDLTimer m where
class (MonadSDLEvents m, MonadSDLTimer m) => MonadSDLVideo m where
