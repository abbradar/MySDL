module Graphics.UI.SDL.Video.Internal.Surface where

import Control.Applicative ((<$>))
import Foreign.ForeignPtr.Safe (ForeignPtr,
                                withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.C.Types (CInt(..))
import Foreign.Storable(Storable(..))
import Foreign.Ptr (Ptr)
import Graphics.Rendering.OpenGL.GL.Tensor (Vector2(..))

#include <SDL2/SDL_surface.h>

{#pointer *SDL_Surface as CSurface foreign newtype #}

data Rect = Rect !(Vector2 Int) !(Vector2 Int)

{#pointer *SDL_Rect as CRect #}

withCRect :: Rect -> (CRect -> IO a) -> IO a
withCRect (Rect (Vector2 x y) (Vector2 w h)) f =
  allocaBytesAligned {#sizeof SDL_Rect #} {#alignof SDL_Rect #} $ \p -> do
    {#set SDL_Rect->x #} p $ fromIntegral x
    {#set SDL_Rect->y #} p $ fromIntegral y
    {#set SDL_Rect->w #} p $ fromIntegral w
    {#set SDL_Rect->h #} p $ fromIntegral h
    f p

crectToRect :: CRect -> IO Rect
crectToRect p = do
  x <- fromIntegral <$> {#get SDL_Rect->x #} p
  y <- fromIntegral <$> {#get SDL_Rect->y #} p
  w <- fromIntegral <$> {#get SDL_Rect->w #} p
  h <- fromIntegral <$> {#get SDL_Rect->h #} p

  return $ Rect (Vector2 x y) (Vector2 w h)
