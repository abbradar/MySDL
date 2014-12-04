{-|
Description: Types of possible SDL events.
-}

{-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.SDL.Events.Types where

-- Almost everything in this module is strict, because we get events
-- "precomputed" from SDL.

-- TODO: Rename all fields marked with "ORF" when OverloadedRecordFields
-- will be out (in GHC 7.10, probably).

import Control.Applicative
import Data.Word
import Data.Int
import Data.Text (Text)
import Foreign.C.Types (CInt(..), CUInt(..), CUChar(..))
import Control.Lens (makePrisms, makeLenses, _2)

import Graphics.UI.SDL.Timer.Ticks
import Graphics.UI.SDL.Video.Keyboard.Types
import Graphics.UI.SDL.Video.Window
import Graphics.UI.SDL.Video.Mouse
import Graphics.UI.SDL.Types

#include <SDL2/SDL_events.h>
#include <SDL2/SDL_mouse.h>

-- We keep all IDs in their C types without conversion.
-- | Index of joystick/gamepad's 'thingy' (button/axis/wheel).
type JoyIndex = CUChar
-- | Touch device ID
type TouchID = CInt
-- TODO: is there any difference at all? I can't understand and I haven't tried
-- actually using joysticks API.
type JoyInstance = CInt
type JoyID = CUInt

type TouchPoint = Point Float

-- | Key state.
{#enum define KeyState { SDL_RELEASED as Released
                       , SDL_PRESSED as Pressed
                       } deriving (Show, Eq) #}

-- | Keyboard event data.
--
-- [@_kstate@] Key state.
-- [@_krepeat@] Is this a repeated keypress event?
-- [@_keySym@] What key emitted an event.
data KeyboardEvent = KeyboardEvent { _kstate :: !KeyState -- ORF
                                   , _krepeat :: !Bool
                                   , _keySym :: !KeySym
                                   }
                   deriving (Eq, Show)

makeLenses ''KeyboardEvent

-- TODO: what do this represent, actually?
-- see http://sdl.5483.n7.nabble.com/Confused-on-how-to-handle-or-why-SDL-StartTextInput-tp36182p36192.html
-- | Text editing event data.
data TextEditingEvent = TextEditingEvent { _tstart :: !Int32
                                         , _tlength :: !Int32
                                         }
                      deriving (Eq, Show)

makeLenses ''TextEditingEvent

-- | Text event data.
data TextEvent = Editing !TextEditingEvent
               | Input
               deriving (Eq, Show)

makePrisms ''TextEvent

-- | Mouse motion event data.
--
-- [@mstates@] Pressed buttons.
-- [@mrel@] Mouse motion relative to previous event.
-- [@mmpos@] Mouse position relative to window.
data MouseMotionEvent = MouseMotionEvent { _mstates :: MouseButtonState -- ORF, this is needed rarely and computed slowly
                                         , _mrel :: !PosPoint -- ORF
                                         , _mmpos :: !PosPoint -- ORF
                                         }
                      deriving (Eq, Show)

makeLenses ''MouseMotionEvent

-- | Mouse button event data.
--
-- [@mbutton@] Mouse button which triggered this event.
-- [@mstate@] Button state.
-- [@clicks@] Number of successive fast clicks.
-- [@mbpos@] Mouse position relative to window.
data MouseButtonEvent = MouseButtonEvent { _mbutton :: !MouseButton -- ORF
                                         , _mstate :: !KeyState -- ORF
                                         , _clicks :: !Word8
                                         , _mbpos :: !PosPoint -- ORF
                                         }
                      deriving (Eq, Show)

makeLenses ''MouseButtonEvent

-- | Mouse event data.
--
-- [@MWheel@] Movement of the 2D wheel relative to previous event.
data MouseEvent = MMotion !MouseMotionEvent
                | MButton !MouseButtonEvent
                | MWheel !PosPoint
                deriving (Eq, Show)

makePrisms ''MouseEvent

-- | Mouse ID.
data WhichMouse = MouseTouch | MouseID !CUInt
                deriving (Show, Eq, Ord)

-- | Window events data.
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

-- | Joystick events data.
--
-- [@Hat@] Position of a hat, with up-right position being (1, 1)
data JoystickEvent = Axis !JoyIndex !Position
                   | Ball !JoyIndex !PosPoint
                   | Hat !JoyIndex !PosPoint
                   | Button !JoyIndex !KeyState
                   | Removed
                   | ControllerAxis !JoyIndex !Position
                   | ControllerButton !JoyIndex !KeyState
                   | ControllerRemoved
                   | ControllerRemapped
                   deriving (Eq, Show)

makePrisms ''JoystickEvent

-- TODO: get more info
-- | Platform-specific window events.
data WMEvent = Windows
             | X11
             | DirectFB
             deriving (Eq, Show)

makePrisms ''WMEvent

-- | Finger touch event data.
--
-- [@finger@] ID of a finger (counted from first which began touching)
-- [@fstate@] Finger state (pressing or removed)
-- [@moving@] Is finger moving?
-- [@tfpos@] Normalized by window size position of a finger.
-- [@trel@] Position of a finger relative to previous position.
data TouchFingerEvent = TouchFingerEvent { finger :: !CInt
                                         , fstate :: !KeyState
                                         , moving :: !Bool
                                         , tfpos :: !TouchPoint -- ORF
                                         , trel :: !TouchPoint -- ORF
                                         , pressure :: !Float
                                         }
                      deriving (Eq, Show)

-- | Simple touch gesture event data.
--
-- [@theta@] Angle by which fingers rotated.
-- [@dist@] Distance of a pinch.
-- [@tgpos@] Normalized center of a gesture.
-- [@gfingers@] Number of fingers.
data TouchGestureEvent = TouchGestureEvent { theta :: !Float
                                           , dist :: !Float
                                           , tgpos :: !TouchPoint -- ORF
                                           , gfingers :: !Int -- ORF
                                           }
                       deriving (Eq, Show)

-- | Custom gesture event data.
--
-- [@gesture@] ID of a gesture.
-- [@dfingers@] Number of fingers.
-- [@gerror@] Error relative to absolute match.
-- [@tdpos@] Normalized center of a gesture.
data TouchDollarEvent = TouchDollarEvent { gesture :: !CInt
                                         , dfingers :: !Int -- ORF
                                         , gerror :: !Float
                                         , tdpos :: !TouchPoint -- ORF
                                         }
                      deriving (Eq, Show)

-- | Touch event data.
data TouchEvent = Finger !TouchFingerEvent
                | Gesture !TouchGestureEvent
                | DollarGesture !TouchDollarEvent
                deriving (Eq, Show)

makePrisms ''TouchEvent

-- | Joystick type.
data JoystickType = IsJoystick | IsGameController
                  deriving (Eq, Show)

-- | SDL incoming event data.
--
-- [@Drop@] Drag-and-dropped text or a file.
data EventData = Window !WindowID !WindowEvent
               | Joystick !JoyInstance !JoystickEvent
               | JoystickAdded !JoyID !JoystickType
               | Quit
               | SysWM !WMEvent
               | Touch !TouchID !TouchEvent
               | Drop !Text
               deriving (Eq, Show)

makePrisms ''EventData

anyWindow :: Applicative f => (WindowEvent -> f WindowEvent) -> EventData -> f EventData
anyWindow = _Window . _2

anyMouse :: Applicative f => (MouseEvent -> f MouseEvent) -> WindowEvent -> f WindowEvent
anyMouse = _Mouse . _2

-- | SDL incoming event.
data SDLEvent = SDLEvent !Ticks !EventData
           deriving (Eq, Show)
