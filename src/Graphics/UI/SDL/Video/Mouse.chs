module Graphics.UI.SDL.Video.Mouse
       ( MouseButton(..)
       , MouseButtonState
       , MousePosition
       , getMouseState
       , getRelativeMouseState
       ) where

import Control.Monad.Base (liftBase)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr)
import Data.Int
import Data.Word
import Graphics.Rendering.OpenGL.GL.Tensor (Vector2(..))

import Graphics.UI.SDL.Class
import Graphics.UI.SDL.Video.Internal.Mouse

#include <SDL2/SDL_mouse.h>

type MousePosition = Vector2 Int32

getMouseState' :: MonadSDLVideo m => IO (Word32, CInt, CInt) -> m (MousePosition, MouseButtonState)
getMouseState' call = liftBase $ do
  (f, (CInt x), (CInt y)) <- call
  return (Vector2 x y, mmaskToButtons f)

getMouseState :: MonadSDLVideo m => m (MousePosition, MouseButtonState)
getMouseState = getMouseState' sDLGetMouseState
  where {#fun unsafe SDL_GetMouseState as ^
         { alloca- `CInt' peek*
         , alloca- `CInt' peek*
         } -> `Word32' #}

getRelativeMouseState :: MonadSDLVideo m => m (MousePosition, MouseButtonState)
getRelativeMouseState = getMouseState' sDLGetRelativeMouseState
  where {#fun unsafe SDL_GetRelativeMouseState as ^
         { alloca- `CInt' peek*
         , alloca- `CInt' peek*
         } -> `Word32' #}
