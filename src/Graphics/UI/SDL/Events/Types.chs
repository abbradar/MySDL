{-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.SDL.Events.Types where

-- Almost everything in this module is strict, because we get events
-- "precomputed" from SDL.

-- TODO: Rename all fields marked with "ORF" when OverloadedRecordFields
-- will be out (in GHC 7.10, probably).

import Data.Word
import Data.Int
import Data.Text (Text)
import Foreign.C.Types (CInt(..), CUInt(..), CUChar(..))
import Control.Lens.TH

import Graphics.UI.SDL.Timer.Types
import Graphics.UI.SDL.Video.Keyboard.Types
import Graphics.UI.SDL.Video.Window
import Graphics.UI.SDL.Video.Mouse
import Graphics.UI.SDL.Types

#include <SDL2/SDL_events.h>
#include <SDL2/SDL_mouse.h>

-- We keep all IDs in their C types without conversion.
type JoyIndex = CUChar
type JoyInstance = CInt
type JoyID = CUInt
type TouchID = CInt

type TouchPoint = Point Float

{#enum define KeyState { SDL_RELEASED as Released
                       , SDL_PRESSED as Pressed
                       } deriving (Show, Eq) #}

data KeyboardEvent = KeyboardEvent { _kstate :: !KeyState -- ORF
                                   , _krepeat :: !Bool
                                   , _keySym :: !KeySym
                                   }
                   deriving (Eq, Show)

makeLenses ''KeyboardEvent

data TextEditingEvent = TextEditingEvent { _tstart :: !Int32
                                         , _tlength :: !Int32
                                         }
                      deriving (Eq, Show)

makeLenses ''TextEditingEvent

data TextEvent = Editing !TextEditingEvent
               | Input
               deriving (Eq, Show)

makePrisms ''TextEvent

data MouseMotionEvent = MouseMotionEvent { mstates :: MouseButtonState -- ORF, this is needed rarely and computed slowly
                                         , mrel :: !MousePosition -- ORF
                                         , mmpos :: !MousePosition -- ORF
                                         }
                      deriving (Eq, Show)

makeLenses ''MouseMotionEvent

data MouseButtonEvent = MouseButtonEvent { mbutton :: !MouseButton -- ORF
                                         , mstate :: !KeyState -- ORF
                                         , clicks :: !Word8
                                         , mbpos :: !MousePosition -- ORF
                                         }
                      deriving (Eq, Show)

makeLenses ''MouseButtonEvent

data MouseEvent = MMotion !MouseMotionEvent
                | MButton !MouseButtonEvent
                | MWheel !PosPoint
                deriving (Eq, Show)

makePrisms ''MouseEvent

data WhichMouse = MouseTouch | MouseID !CUInt
                deriving (Show, Eq, Ord)

data WindowEvent = Shown
                 | Hidden
                 | Exposed
                 | Moved !PosPoint
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
                   | Ball !JoyIndex !PosPoint
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
                                         , tfpos :: !TouchPoint -- ORF
                                         , trel :: !TouchPoint -- ORF
                                         , pressure :: !Float
                                         }
                      deriving (Eq, Show)

data TouchGestureEvent = TouchGestureEvent { theta :: !Float
                                           , dist :: !Float
                                           , tgpos :: !TouchPoint -- ORF
                                           , gfingers :: !Word16
                                           }
                       deriving (Eq, Show)

data TouchDollarEvent = TouchDollarEvent { gesture :: !CInt
                                         , dfingers :: !Word32
                                         , gerror :: !Float
                                         , tdpos :: !TouchPoint -- ORF
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
