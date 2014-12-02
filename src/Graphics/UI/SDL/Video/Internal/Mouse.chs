module Graphics.UI.SDL.Video.Internal.Mouse
       ( MouseButton
       , mouseLeft
       , mouseMiddle
       , mouseRight
       , mouseX1
       , mouseX2
       , MouseButtonState
       , mmaskToButtons
       ) where

import Foreign.C.Types (CUInt(..))
import Data.Bits ((.&.))
import Data.Word
import Data.Set (Set)
import qualified Data.Set as S

import Data.Enum.Num

#include <SDL2/SDL_mouse.h>
#include "sdl_misc.h"

-- TODO: replace with {#const #} when avaliable
{#enum define CMouseButton { SDL_BUTTON_LEFT as MouseLeft
                           , SDL_BUTTON_MIDDLE as MouseMiddle
                           , SDL_BUTTON_RIGHT as MouseRight
                           , SDL_BUTTON_X1 as MouseX1
                           , SDL_BUTTON_X2 as MouseX2
                           } #}

-- | Mouse button (enumerated from zero) in range [0..32).
newtype MouseButton = MouseButton Word8
                    deriving (Eq, Show, Ord)

instance Enum MouseButton where
  fromEnum (MouseButton x) = fromEnum x
  toEnum x
    | x >= 0 && x < 32 = MouseButton $ fromIntegral x
    | otherwise = error "toEnum: MouseButton should be [0..32)"

instance Bounded MouseButton where
  minBound = MouseButton 0
  maxBound = MouseButton 31

-- | Left mouse button.
mouseLeft :: MouseButton
mouseLeft = MouseButton $ fromEnum' MouseLeft

-- | Middle mouse button.
mouseMiddle :: MouseButton
mouseMiddle = MouseButton $ fromEnum' MouseMiddle

-- | Right mouse button.
mouseRight :: MouseButton
mouseRight = MouseButton $ fromEnum' MouseRight

-- | X1 mouse button.
mouseX1 :: MouseButton
mouseX1 = MouseButton $ fromEnum' MouseX1

-- | X2 mouse button.
mouseX2 :: MouseButton
mouseX2 = MouseButton $ fromEnum' MouseX2

type MouseButtonState = Set MouseButton

mmaskToButtons :: Word32 -> MouseButtonState
mmaskToButtons bf = S.fromList $ map snd $ filter (\(m, _) -> bf .&. m /= 0) $
                    map (\a -> (getMask $ fromEnum' a, a)) [minBound..maxBound]
  where {#fun pure unsafe get_mask as ^ { `Word32' } -> `Word32' #}
