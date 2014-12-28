{-|
Description: SDL window surface management.
-}

module Graphics.UI.SDL.Video.Surface
       ( Rect(..)
       , updateWindowSurface
       , getWindowSurface
       ) where

import Control.Monad
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr
import Foreign.Ptr (Ptr)
import Control.Monad.IO.ExClass

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Video.Window

{#import Graphics.UI.SDL.Video.Internal.Window #}
{#import Graphics.UI.SDL.Video.Internal.Surface #}

#include <SDL2/SDL_video.h>

-- | Update window with its binded surface.
updateWindowSurface :: MonadIO' m => Window -> m ()
updateWindowSurface (Window w) = liftIO $ sdlCode "SDL_UpdateWindowSurface" $ sDLUpdateWindowSurface w
  where {#fun unsafe SDL_UpdateWindowSurface as ^
         { `CWindow' } -> `Int' #}

type Surface = CSurface

-- | Get a surface binded to a window.
getWindowSurface :: MonadIO' m => Window -> m Surface
getWindowSurface (Window w) = liftIO $ sdlObject "SDL_GetWindowSurface" (\case CSurface a -> a) $ sDLGetWindowSurface w
  where {#fun unsafe SDL_GetWindowSurface as ^
         { `CWindow' } -> `CSurface' #}
