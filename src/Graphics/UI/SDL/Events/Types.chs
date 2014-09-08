{-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.SDL.Events.Types where

-- TODO: Rename all fields marked with "*" when OverloadedRecordFields
-- will be out (in GHC 7.10, probably).

import Data.Word
import Data.Int
import Data.Text (Text)
import Graphics.Rendering.OpenGL.GL.Tensor (Vector2(..))
import Foreign.C.Types (CInt(..), CUInt(..), CUChar(..))
import Control.Lens.TH

import Graphics.UI.SDL.Timer.Types
import Graphics.UI.SDL.Video.Keyboard.Types
import Graphics.UI.SDL.Video.Window
import Graphics.UI.SDL.Video.Mouse

#include <SDL2/SDL_events.h>
#include <SDL2/SDL_mouse.h>
  
type JoyIndex = CUChar
type Position = Int32

{#enum define KeyState { SDL_RELEASED as Released
                       , SDL_PRESSED as Pressed
                       } deriving (Show, Eq) #}

data KeyboardEvent = KeyboardEvent { _kstate :: !KeyState -- *
                                   , _krepeat :: !Bool
                                   , _keySym :: !KeySym
                                   }
                   deriving (Eq, Show)

makeLenses ''KeyboardEvent

type JoyInstance = CInt
type JoyID = CUInt
type TouchID = CInt

type PosVec = Vector2 Position
type TouchPosVec = Vector2 Float

data TextEditingEvent = TextEditingEvent { _tstart :: !Int32
                                         , _tlength :: !Int32
                                         }
                        deriving (Eq, Show)

makeLenses ''TextEditingEvent

data TextEvent = Editing !TextEditingEvent
               | Input
               deriving (Eq, Show)

makePrisms ''TextEvent

data MouseMotionEvent = MouseMotionEvent { mstates :: MouseButtonState -- *, this is needed rarely and computed slowly
                                         , mrel :: !MousePosition -- *
                                         , mmpos :: !MousePosition -- *
                                         }
                      deriving (Eq, Show)

makeLenses ''MouseMotionEvent

data MouseButtonEvent = MouseButtonEvent { mbutton :: !MouseButton -- *
                                         , mstate :: !KeyState -- *
                                         , clicks :: !Word8
                                         , mbpos :: !MousePosition -- *
                                         }
                      deriving (Eq, Show)

makeLenses ''MouseButtonEvent

data MouseEvent = MMotion !MouseMotionEvent
                | MButton !MouseButtonEvent
                | MWheel !PosVec
                deriving (Eq, Show)

makePrisms ''MouseEvent

data WhichMouse = MouseTouch | MouseID !CUInt
                deriving (Show, Eq, Ord)

data WindowEvent = Shown
                 | Hidden
                 | Exposed
                 | Moved !Point
                 | Resized !Size
                 | SizeChanged !Size
                 | Minimized
                 | Maximized
                 | Restored
                 | WinEntered
                 | WinLeft
                 | FocusGained
                 | FocusLost
                 | Closed
                 | Keyboard !KeyboardEvent
                 | Text !Text !TextEvent
                 | Mouse !WhichMouse !MouseEvent
                 deriving (Show, Eq)

makePrisms ''WindowEvent

data XAxis = East | XCenter | West
           deriving (Eq, Show, Ord)

data YAxis = North | YCenter | South
           deriving (Eq, Show, Ord)

data JoyHat = JoyHat !XAxis !YAxis
           deriving (Eq, Show)

data JoystickEvent = Axis !JoyIndex !Position
                   | Ball !JoyIndex !PosVec
                   | Hat !JoyIndex !JoyHat
                   | Button !JoyIndex !KeyState
                   | Removed
                   | ControllerAxis !JoyIndex !Position
                   | ControllerButton !JoyIndex !KeyState
                   | ControllerRemoved
                   | ControllerRemapped
                   deriving (Eq, Show)

makePrisms ''JoystickEvent

-- TODO: Get more info
data WMEvent = Windows
             | X11
             | DirectFB
             deriving (Eq, Show)

makePrisms ''WMEvent

data TouchFingerEvent = TouchFingerEvent { finger :: !CInt
                                         , fstate :: !KeyState
                                         , moving :: !Bool
                                         , tfpos :: !TouchPosVec -- *
                                         , trel :: !TouchPosVec -- *
                                         , pressure :: !Float
                                         }
                      deriving (Eq, Show)

data TouchGestureEvent = TouchGestureEvent { theta :: !Float
                                           , dist :: !Float
                                           , tgpos :: !TouchPosVec -- *
                                           , gfingers :: !Word16
                                           }
                       deriving (Eq, Show)

data TouchDollarEvent = TouchDollarEvent { gesture :: !CInt
                                         , dfingers :: !Word32
                                         , gerror :: !Float
                                         , tdpos :: !TouchPosVec -- *
                                         }
                      deriving (Eq, Show)

data TouchEvent = Finger !TouchFingerEvent
                | Gesture !TouchGestureEvent
                | DollarGesture !TouchDollarEvent
                deriving (Eq, Show)

makePrisms ''TouchEvent

data JoystickType = IsJoystick | IsGameController
                  deriving (Eq, Show)

data EventData = Window !WindowID !WindowEvent
               | Joystick !JoyInstance !JoystickEvent
               | JoystickAdded !JoyID !JoystickType
               | Quit
               | SysWM !WMEvent
               | Touch !TouchID !TouchEvent
               | Drop !Text
               deriving (Eq, Show)

makePrisms ''EventData

data Event = Event !Ticks !EventData
           deriving (Show, Eq)
