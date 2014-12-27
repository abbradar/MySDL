{-|
Description: Generic mouse control.
-}

module Graphics.UI.SDL.Video.Mouse
       ( MouseButton
       , MouseButtonState
       , getMouseState
       , getRelativeMouseState
       , getMouseGrab
       , setMouseGrab
       , getCursorShown
       , setCursorShown
       ) where

import Control.Monad
import Control.Applicative
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils
import Data.Word
import Control.Monad.IO.Class

import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Video.Monad
import Graphics.UI.SDL.Video.Internal.Mouse

{#import Graphics.UI.SDL.Video.Internal.Window #}
{#import Graphics.UI.SDL.Internal.Prim #}

#include <SDL2/SDL_mouse.h>

getMouseState' :: MonadSDLVideo m => IO (Word32, CInt, CInt) -> m (PosPoint, MouseButtonState)
getMouseState' call = liftIO $ do
  (f, (CInt x), (CInt y)) <- call
  return (V2 x y, mmaskToButtons f)

-- | Get mouse state (absolute for the screen space).
getMouseState :: MonadSDLVideo m => m (PosPoint, MouseButtonState)
getMouseState = getMouseState' sDLGetMouseState
  where {#fun unsafe SDL_GetMouseState as ^
         { alloca- `CInt' peek*
         , alloca- `CInt' peek*
         } -> `Word32' #}

-- | Get relative mouse state regarding to the last call.
getRelativeMouseState :: MonadSDLVideo m => m (PosPoint, MouseButtonState)
getRelativeMouseState = getMouseState' sDLGetRelativeMouseState
  where {#fun unsafe SDL_GetRelativeMouseState as ^
         { alloca- `CInt' peek*
         , alloca- `CInt' peek*
         } -> `Word32' #}

-- | Check if mouse cursor is grabbed by a window.
getMouseGrab :: (MonadSDLVideo m, SDLWindow w) => w -> m Bool
getMouseGrab (toCWindow -> w) = liftIO $ fromSDLBool <$> sDLGetWindowGrab w
  where {#fun unsafe SDL_GetWindowGrab as ^
         { `CWindow' } -> `SDLBool' #}

-- | Grab or ungrab mouse cursor.
setMouseGrab :: (MonadSDLVideo m, SDLWindow w) => Bool -> w -> m ()
setMouseGrab (toSDLBool -> s) (toCWindow -> w) = liftIO $ sDLSetWindowGrab w s
  where {#fun unsafe SDL_SetWindowGrab as ^
         { `CWindow', `SDLBool' } -> `()' #}

showCursor' :: MonadSDLVideo m => Int -> m Int
showCursor' i = liftIO $ sdlCall "SDL_ShowCursor" (sDLShowCursor i) (/= -1)
  where {#fun unsafe SDL_ShowCursor as ^
         { `Int' } -> `Int' #}

-- | Check if mouse cursor is hidden (globally).
getCursorShown :: MonadSDLVideo m => m Bool
getCursorShown = toBool <$> showCursor' (-1)

-- | Hide or show a cursor.
setCursorShown :: MonadSDLVideo m => Bool -> m ()
setCursorShown = void . showCursor' . fromBool
