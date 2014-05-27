module Graphics.UI.SDL.Video.ScreenSaver (
  enableScreenSaver,
  disableScreenSaver,
  isScreenSaverEnabled
  ) where

import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Utils (toBool)
import Control.Monad.Base (liftBase)

import Graphics.UI.SDL.Video

#include <SDL2/SDL_video.h>

enableScreenSaver :: MonadSDLVideo m => m ()
enableScreenSaver = liftBase {#call unsafe SDL_EnableScreenSaver as ^ #}

disableScreenSaver :: MonadSDLVideo m => m ()
disableScreenSaver = liftBase {#call unsafe SDL_DisableScreenSaver as ^ #}

isScreenSaverEnabled :: MonadSDLVideo m => m Bool
isScreenSaverEnabled = liftBase $ sDLIsScreenSaverEnabled
  where {#fun unsafe SDL_IsScreenSaverEnabled as ^ {} -> `Bool' #}
