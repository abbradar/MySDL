module Graphics.UI.SDL.Video.Internal.GLContextFlag where

-- This is moved to separate module for TH to work

#include <SDL2/SDL_video.h>

{#enum SDL_GLcontextFlag as GLContextFlag {underscoreToCase} deriving (Show, Eq) #}
