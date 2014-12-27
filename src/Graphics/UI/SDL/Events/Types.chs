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
-- | Touch device ID.
type TouchID = CInt
-- TODO: is there any difference at all? I can't understand and I haven't tried
-- actually using joysticks API.
type JoyInstance = CInt
type JoyID = CUInt

-- | Normalized touch point.
type TouchPoint = V2 Float

-- | Key state.
{#enum define KeyState { SDL_RELEASED as Released
                       , SDL_PRESSED as Pressed
                       } deriving (Show, Eq) #}

-- | Keyboard event data.
--
data KeyboardEvent = KeyboardEvent { -- | Key state.
                                     _kstate :: !KeyState -- ORF
                                     -- | Is this a repeated keypress event?
                                   , _krepeat :: !Bool
                                     -- | What key emitted an event.
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
data MouseMotionEvent = MouseMotionEvent { -- | Pressed buttons.
                                           _mstates :: MouseButtonState -- ORF, this is needed rarely and computed slowly
                                           -- | Mouse motion relative to previous event.
                                         , _mrel :: !PosPoint -- ORF
                                           -- | Mouse position relative to window.
                                         , _mmpos :: !PosPoint -- ORF
                                         }
                      deriving (Eq, Show)

makeLenses ''MouseMotionEvent

-- | Mouse button event data.
data MouseButtonEvent = MouseButtonEvent { -- | Mouse button which triggered this event.
                                           _mbutton :: !MouseButton -- ORF
                                           -- | Button state.
                                         , _mstate :: !KeyState -- ORF
                                           -- | Number of successive fast clicks.
                                         , _clicks :: !Word8
                                           -- | Mouse position relative to window.
                                         , _mbpos :: !PosPoint -- ORF
                                         }
                      deriving (Eq, Show)

makeLenses ''MouseButtonEvent

-- | Mouse event data.
data MouseEvent = MMotion !MouseMotionEvent
                | MButton !MouseButtonEvent
                | MWheel !PosPoint -- | Movement of the 2D wheel relative to previous event.
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
data JoystickEvent = Axis !JoyIndex !Position
                   | Ball !JoyIndex !PosPoint
                   | Hat !JoyIndex !PosPoint -- | Position of a hat, with up-right position being (1, 1).
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
data TouchFingerEvent = TouchFingerEvent { -- | ID of a finger (counted from first which began touching).
                                           finger :: !CInt
                                           -- | Finger state (pressing or removed).
                                         , fstate :: !KeyState
                                           -- | Is finger moving?
                                         , moving :: !Bool
                                           -- | Absolute position of a finger.
                                         , tfpos :: !TouchPoint -- ORF
                                           -- | Position of a finger relative to previous position.
                                         , trel :: !TouchPoint -- ORF
                                           -- | Normalized pressure.
                                         , pressure :: !Float
                                         }
                      deriving (Eq, Show)

-- | Simple touch gesture event data.
data TouchGestureEvent = TouchGestureEvent { -- | Angle by which fingers rotated.
                                             theta :: !Float
                                             -- | Distance of a pinch.
                                           , dist :: !Float
                                             -- | Normalized center of a gesture.
                                           , tgpos :: !TouchPoint -- ORF
                                             -- | Number of fingers.
                                           , gfingers :: !Int -- ORF
                                           }
                       deriving (Eq, Show)

-- | Custom gesture event data.
--
data TouchDollarEvent = TouchDollarEvent { -- | ID of a gesture.
                                           gesture :: !CInt
                                           -- | Number of fingers.
                                         , dfingers :: !Int -- ORF
                                           -- | Error relative to absolute match.
                                         , gerror :: !Float
                                           -- | Normalized center of a gesture.
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
data EventData = Window !WindowID !WindowEvent
               | Joystick !JoyInstance !JoystickEvent
               | JoystickAdded !JoyID !JoystickType
               | Quit
               | SysWM !WMEvent
               | Touch !TouchID !TouchEvent
               | Drop !Text -- | Drag-and-dropped text or a file.
               deriving (Eq, Show)

makePrisms ''EventData

anyWindow :: Applicative f => (WindowEvent -> f WindowEvent) -> EventData -> f EventData
anyWindow = _Window . _2

anyMouse :: Applicative f => (MouseEvent -> f MouseEvent) -> WindowEvent -> f WindowEvent
anyMouse = _Mouse . _2

-- | SDL incoming event.
data SDLEvent = SDLEvent !Ticks !EventData
           deriving (Eq, Show)
