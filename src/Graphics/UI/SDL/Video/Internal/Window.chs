module Graphics.UI.SDL.Video.Internal.Window
       ( CWindow(..)
       , withCWindow
       ) where

import Foreign.ForeignPtr.Safe (ForeignPtr,
                                withForeignPtr)
import Foreign.Ptr (Ptr)

#include <SDL2/SDL_video.h>

{#pointer *SDL_Window as CWindow foreign newtype #}
