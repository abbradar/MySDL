{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.UI.SDL.Video.Surface
       ( Rect(..)
       , updateWindowSurface
       , getWindowSurface
       ) where

import Control.Monad
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr.Safe (newForeignPtr_)
import Foreign.Ptr (Ptr)
import Control.Monad.Base (liftBase)

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Video.Monad
import Graphics.UI.SDL.Video.Window

{#import Graphics.UI.SDL.Video.Internal.Window #}
{#import Graphics.UI.SDL.Video.Internal.Surface #}

#include <SDL2/SDL_video.h>

updateWindowSurface :: MonadSDLVideo m => Window -> m ()
updateWindowSurface = liftBase . sdlCode "SDL_UpdateWindowSurface" . sDLUpdateWindowSurface
  where {#fun unsafe SDL_UpdateWindowSurface as ^
         { `CWindow' } -> `Int' #}

type Surface = CSurface

getWindowSurface :: MonadSDLVideo m => Window -> m Surface
getWindowSurface = liftBase . sdlObject "SDL_GetWindowSurface" (\case CSurface a -> a) . sDLGetWindowSurface
  where {#fun unsafe SDL_GetWindowSurface as ^
         { `CWindow' } -> `CSurface' #}
