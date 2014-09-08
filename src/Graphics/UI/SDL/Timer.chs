module Graphics.UI.SDL.Timer
       ( getTicks
       ) where

import Control.Applicative ((<$>))
import Foreign.C.Types (CUInt(..))
import Control.Monad.Base (liftBase)

import Graphics.UI.SDL.Timer.Types
import Graphics.UI.SDL.Class

#include <SDL2/SDL_timer.h>

-- This is meant only for relative comparison.
getTicks :: MonadSDL m => m Ticks
getTicks = liftBase $ fromIntegral <$> {#call unsafe SDL_GetTicks as ^ #}
