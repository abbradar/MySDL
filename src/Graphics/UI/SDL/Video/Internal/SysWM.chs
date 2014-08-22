module Graphics.UI.SDL.Video.Internal.SysWM
       ( CSysWMType(..)
       ) where

#include <SDL2/SDL_syswm.h>

{#enum SDL_SYSWM_TYPE as CSysWMType {underscoreToCase} 
   deriving (Show, Eq) #}
