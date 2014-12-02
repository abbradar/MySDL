{-|
Description: Generic mouse control.
-}

module Graphics.UI.SDL.Video.Mouse
       ( MouseButton
       , MouseButtonState
       , getMouseState
       , getRelativeMouseState
       ) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr)
import Data.Word
import Control.Monad.IO.Class

import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Video.Monad
import Graphics.UI.SDL.Video.Internal.Mouse

#include <SDL2/SDL_mouse.h>

getMouseState' :: MonadSDLVideo m => IO (Word32, CInt, CInt) -> m (PosPoint, MouseButtonState)
getMouseState' call = liftIO $ do
  (f, (CInt x), (CInt y)) <- call
  return (P x y, mmaskToButtons f)

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
