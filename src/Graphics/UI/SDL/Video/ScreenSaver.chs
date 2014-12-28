{-|
Description: Screen saver management.
-}

module Graphics.UI.SDL.Video.ScreenSaver
       ( enableScreenSaver
       , disableScreenSaver
       , isScreenSaverEnabled
       ) where

import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Utils (toBool)
import Control.Monad.IO.ExClass


#include <SDL2/SDL_video.h>

-- | Enable inhibition of screen saver.
disableScreenSaver :: MonadIO' m => m ()
disableScreenSaver = liftIO {#call unsafe SDL_DisableScreenSaver as ^ #}

-- | Disable inhibition of screen saver.
enableScreenSaver :: MonadIO' m => m ()
enableScreenSaver = liftIO {#call unsafe SDL_EnableScreenSaver as ^ #}

-- | Check if screen saver is inhibited.
isScreenSaverEnabled :: MonadIO' m => m Bool
isScreenSaverEnabled = liftIO $ sDLIsScreenSaverEnabled
  where {#fun unsafe SDL_IsScreenSaverEnabled as ^ {} -> `Bool' #}
