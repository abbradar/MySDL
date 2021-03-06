{-|
Description: Various enumerations for keyboard support (keys, modifiers etc.)
-}

{-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.SDL.Video.Keyboard.Types where

import Control.Lens.TH (makeLenses)
import Data.BitSet.Word (BitSet)

#include <SDL2/SDL_keyboard.h>
#include <SDL2/SDL_keycode.h>

-- | Key codes (virtual keys). A code may be SdlkUnknown when there is no
--   corresponding entry in translation table to convert a scancode.
{#enum SDLK_UNKNOWN as KeyCode {underscoreToCase} deriving (Eq, Show, Ord) #}

-- | Scancodes (hardware key codes).
{#enum SDL_Scancode as ScanCode {underscoreToCase} deriving (Eq, Show, Ord) #}

-- | Modifier keys.
data KeyMod = ModLShift
            | ModRShift
            | ModLCtrl
            | ModRCtrl
            | ModLAlt
            | ModRAlt
            | ModLGUI
            | ModRGUI
            | ModNum
            | ModCaps
            | ModMode
            deriving (Eq, Show, Ord, Enum)

-- | Key symbol (everything available about a key, including modifiers).
data KeySym = KeySym { _scanCode :: !ScanCode
                     , _keyCode :: !KeyCode
                     , _keyMod :: !(BitSet KeyMod)
                     }
            deriving (Eq, Show)

makeLenses ''KeySym

