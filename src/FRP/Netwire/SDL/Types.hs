{-|
Description: High-level SDL state (types only).

Provides high-level SDL state types and lenses/prisms.
-}

{-# LANGUAGE TemplateHaskell #-}

module FRP.Netwire.SDL.Types where

import qualified Data.BitSet.Word as BW
import Data.Map.Strict (Map)
import Data.Set (Set)
import Control.Lens

import Graphics.UI.SDL

-- | Mouse state:
-- [@_mousePressed@] Pressed mouse buttons.
-- [@_mousePos@] Mouse position.
data MouseState = MouseState { _mousePressed :: MouseButtonState
                             , _mousePos :: PosPoint
                             }
                deriving (Show)

makeLenses ''MouseState

-- | Window state:
--
-- [@_keysPressed@] Pressed keys, by keycode.
-- [@_scansPressed@] Pressed keys, by scancode.
-- [@_modsPressed@] Pressed modifier buttons.
-- [@_mouseState@] Mouses' state.
-- [@_shown@] Is window currently shown?
-- [@_pos@] Window position.
-- [@_size@] Window size.
-- [@_mouseFocus@] Does window have mouse focus?
-- [@_kbdFocus@] Does window have keyboard focus?
data WindowState = WindowState { _keysPressed :: Set KeyCode
                               , _scansPressed :: Set ScanCode
                               , _modsPressed :: BW.BitSet KeyMod
                               , _mouseState :: Map WhichMouse MouseState
                               , _wshown :: Bool
                               , _wpos :: PosPoint
                               , _wsize :: Size
                               , _mouseFocus :: Bool
                               , _kbdFocus :: Bool
                               }
                   deriving (Show)

makeLenses ''WindowState

-- | State for the last moment of time:
--
-- [@_rawEvents@] Last received events.
-- [@_windowState@] State by windows.
data StateData = StateData { _rawEvents :: [EventData]
                           , _windowState :: Map WindowID WindowState
                             -- TODO: Add joystick and touch state
                           }
               deriving (Show)

makeLenses ''StateData
