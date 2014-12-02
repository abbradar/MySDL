module Graphics.UI.SDL.Internal.Class where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Catch

-- | SDL core class.
class (Applicative m, MonadIO m, MonadMask m) => MonadSDL m where
-- | SDL audio class.
class MonadSDL m => MonadSDLAudio m where
-- | SDL events class.
class MonadSDL m => MonadSDLEvents m where
-- | SDL timer class.
class MonadSDL m => MonadSDLTimer m where
-- | SDL video class.
class (MonadSDLEvents m, MonadSDLTimer m) => MonadSDLVideo m where
