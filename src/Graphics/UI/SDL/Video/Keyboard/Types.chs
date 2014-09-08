{-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.SDL.Video.Keyboard.Types where

import Control.Lens.TH (makeLenses)
import Data.BitSet.Word (BitSet)

#include <SDL2/SDL_keyboard.h>
#include <SDL2/SDL_keycode.h>

{#enum SDLK_UNKNOWN as KeyCode {underscoreToCase} deriving (Eq, Show, Ord) #}

{#enum SDL_Scancode as ScanCode {underscoreToCase} deriving (Eq, Show, Ord) #}

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

data KeySym = KeySym { _scanCode :: !ScanCode
                     , _keyCode :: !KeyCode
                     , _keyMod :: !(BitSet KeyMod)
                     }
            deriving (Eq, Show)

makeLenses ''KeySym

