module Graphics.UI.SDL.Timer
       ( getTicks
       ) where

import Control.Applicative ((<$>))
import Foreign.C.Types (CUInt(..))
import Control.Monad.Base (liftBase)

import Graphics.UI.SDL.Class
import Graphics.UI.SDL.Timer.Types
import Graphics.UI.SDL.Timer.Internal

#include <SDL2/SDL_timer.h>

getTicks :: MonadSDL m => m Ticks
getTicks = liftBase $ fromIntegral <$> {#call SDL_GetTicks as ^ #} >>= getAbsTicks
