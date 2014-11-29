module Graphics.UI.SDL.Video.Internal.Surface where

import Foreign.ForeignPtr.Safe (ForeignPtr,
                                withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.C.Types (CInt(..))
import Foreign.Storable(Storable(..))
import Foreign.Ptr (Ptr)
import Data.Int

import Graphics.UI.SDL.Types

#include <SDL2/SDL_surface.h>

{#pointer *SDL_Surface as CSurface foreign newtype #}

data Rect = Rect !(Point Int32) !(Point Int32)

{#pointer *SDL_Rect as CRect #}

withCRect :: Rect -> (CRect -> IO a) -> IO a
withCRect (Rect (P x y) (P w h)) f =
  allocaBytesAligned {#sizeof SDL_Rect #} {#alignof SDL_Rect #} $ \p -> do
    {#set SDL_Rect->x #} p $ CInt x
    {#set SDL_Rect->y #} p $ CInt y
    {#set SDL_Rect->w #} p $ CInt w
    {#set SDL_Rect->h #} p $ CInt h
    f p

crectToRect :: CRect -> IO Rect
crectToRect p = do
  CInt x <- {#get SDL_Rect->x #} p
  CInt y <- {#get SDL_Rect->y #} p
  CInt w <- {#get SDL_Rect->w #} p
  CInt h <- {#get SDL_Rect->h #} p

  return $ Rect (P x y) (P w h)
