{-# LANGUAGE TemplateHaskell #-}

module FRP.Netwire.SDL.Types where

import qualified Data.BitSet.Word as BW
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Lens

import Graphics.UI.SDL.Events.Types
import Graphics.UI.SDL.Video.Mouse
import Graphics.UI.SDL.Video.Window
import Graphics.UI.SDL.Video.Keyboard.Types

data MouseState = MouseState { _mousePressed :: MouseButtonState
                             , _mousePos :: MousePosition
                             }
                deriving (Show)

makeLenses ''MouseState

data WindowState = WindowState { _keysPressed :: Set KeyCode
                               , _scansPressed :: Set ScanCode
                               , _modsPressed :: BW.BitSet KeyMod
                               , _mouseState :: Map WhichMouse MouseState
                               , _shown :: Bool
                               , _pos :: Point
                               , _size :: Size
                               , _mouseFocus :: Bool
                               , _kbdFocus :: Bool
                               }
                   deriving (Show)

makeLenses ''WindowState

data StateData = StateData { _rawEvents :: [EventData]
                           , _windowState :: Map WindowID WindowState
                           -- TODO: Add joystick and touch state
                           }
               deriving (Show)

makeLenses ''StateData
