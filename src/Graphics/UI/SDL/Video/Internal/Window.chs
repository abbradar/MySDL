module Graphics.UI.SDL.Video.Internal.Window where

import Foreign.ForeignPtr.Safe (ForeignPtr)
import Foreign.C.Types (CUInt)

#include <SDL2/SDL_video.h>

{#pointer *SDL_Window as CWindow foreign #}

-- | A window without direct OpenGL acceleration (SDL may implement acceleration for 2D surfaces, however).
newtype Window = Window CWindow
-- | A window accelerated with OpenGL.
newtype GLWindow = GLWindow CWindow
-- | Generic window.
type SomeWindow = Either Window GLWindow

-- | Class of SDL windows.
class SDLWindow a where
  toCWindow :: a -> CWindow
  toSomeWindow :: a -> SomeWindow

instance SDLWindow Window where
  toCWindow (Window w) = w
  toSomeWindow = Left

instance SDLWindow GLWindow where
  toCWindow (GLWindow w) = w
  toSomeWindow = Right

instance SDLWindow SomeWindow where
  toCWindow (Left a) = toCWindow a
  toCWindow (Right a) = toCWindow a
  toSomeWindow = id

-- | Internal SDL window ID.
newtype WindowID = WindowID CUInt
                 deriving (Eq, Show, Ord)
