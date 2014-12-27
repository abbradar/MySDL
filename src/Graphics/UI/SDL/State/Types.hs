{-|
Description: High-level SDL state (types only).

Provides high-level SDL state types and lenses/prisms.
-}

{-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.SDL.State.Types where

import qualified Data.BitSet.Word as BW
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Control.Lens

import Graphics.UI.SDL

-- | Mouse state.
data MouseState = MouseState { -- | Pressed mouse buttons.
                               _mousePressed :: MouseButtonState
                               -- | Mouse position.
                             , _mousePos :: PosPoint
                             }
                deriving (Show)

makeLenses ''MouseState

-- | Window state.
data WindowState = WindowState { -- | Pressed keys, by keycode.
                                 _keysPressed :: Set KeyCode
                                 -- | Pressed keys, by scancode.
                               , _scansPressed :: Set ScanCode
                                 -- | Pressed modifier buttons.
                               , _modsPressed :: BW.BitSet KeyMod
                                 -- | Mouses' state.
                               , _mouseState :: Map WhichMouse MouseState
                                 -- | Is window currently shown?
                               , _wshown :: Bool
                                 -- | Window position.
                               , _wpos :: PosPoint
                                 -- | Window size.
                               , _wsize :: Size
                                 -- | Does window have mouse focus?
                               , _mouseFocus :: Bool
                                 -- | window have keyboard focus?
                               , _kbdFocus :: Bool
                               }
                   deriving (Show)

makeLenses ''WindowState

-- | State for the last moment of time.
data StateData = StateData { -- | Last received events.
                             _rawEvents :: [EventData]
                             -- | State by windows.
                           , _windowState :: Map WindowID WindowState
                             -- TODO: Add joystick and touch state
                           }
               deriving (Show)

makeLenses ''StateData

emptyState :: StateData
emptyState = StateData { _rawEvents = []
                       , _windowState = M.empty
                       }
