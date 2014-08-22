module Graphics.UI.SDL.Events.Types where

-- TODO: Rename all fields marked with "*" when OverloadedRecordFields
-- will be out (in GHC 7.10, probably).

import Data.Word
import Data.Int
import Data.Text (Text)
import Data.BitSet.Word (BitSet)
import Graphics.Rendering.OpenGL.GL.Tensor (Vector2(..))

import Graphics.UI.SDL.Timer.Types
import Graphics.UI.SDL.Video.Keyboard

#include <SDL2/SDL_events.h>
#include <SDL2/SDL_mouse.h>
  
type WindowID = Word32
type Index = Word8
type Position = Int
type JoyPos = Int16

{#enum define KeyState { SDL_RELEASED as Released
                       , SDL_PRESSED as Pressed
                       } deriving (Show, Eq) #}

data WindowEvent = Shown
                 | Hidden
                 | Exposed
                 | Moved { wpos :: !PosVec } -- *
                 | Resized { size :: !PosVec }
                 | SizeChanged
                 | Minimized
                 | Maximized
                 | Restored
                 | WinEntered
                 | WinLeft
                 | FocusGained
                 | FocusLost
                 | Closed
                 | Keyboard { kstate :: !KeyState -- *
                            , krepeat :: !Bool
                            , keySym :: !KeySym
                            }
                 | Text !Text !TextEvent
                 | Mouse !WhichMouse !MouseEvent
                 deriving (Show, Eq)

data WhichMouse = MouseTouch | MouseID !Word32
                deriving (Show, Eq)
type JoyInstance = Word32
type JoyID = Word32
type TouchID = Word32

type PosVec = Vector2 Position
type JoyPosVec = Vector2 Position
type TouchPosVec = Vector2 Float

{#enum define MouseButton { SDL_BUTTON_LEFT as MouseLeft
                          , SDL_BUTTON_MIDDLE as MouseMiddle
                          , SDL_BUTTON_RIGHT as MouseRight
                          , SDL_BUTTON_X1 as MouseX1
                          , SDL_BUTTON_X2 as MouseX2
                          } deriving (Show, Eq) #}

data TextEvent = Editing { tstart :: !Int
                         , tlength :: !Int
                         }
               | Input
               deriving (Eq, Show)

data MouseEvent = MMotion { mstates :: !(BitSet MouseButton) -- *
                          , mpos :: !PosVec -- *
                          , mrel :: !PosVec -- *
                          }
                | MButton { mbutton :: !MouseButton -- *
                          , mstate :: !KeyState -- *
                          , clicks :: !Word8
                          , mpos :: !PosVec -- *
                          }
                | MWheel { mrel :: !PosVec } -- *
                deriving (Eq, Show)

data XAxis = East | XCenter | West
           deriving (Eq, Show, Ord)
data YAxis = North | YCenter | South
           deriving (Eq, Show, Ord)

data JoyHat = JoyHat !XAxis !YAxis
           deriving (Eq, Show)

data JoystickEvent = Axis { axis :: !Index
                          , japos :: !JoyPos -- *
                          }
                   | Ball { ball :: !Index
                          , jbrel :: !JoyPosVec -- *
                          }
                   | Hat { hat :: !Index
                         , value :: !JoyHat
                         }
                   | Button { jbutton :: !Index -- *
                            , jbstate :: !KeyState -- *
                            }
                   | Removed
                   | ControllerAxis { axis :: !Index
                                    , cpos :: !JoyPos -- *
                                    }
                   | ControllerButton { cbutton :: !Index -- *
                                      , cstate :: !KeyState -- *
                                      }
                   | ControllerRemoved
                   | ControllerRemapped
                   deriving (Eq, Show)

-- TODO: Get more info
data WMEvent = Windows
             | X11
             | DirectFB
             deriving (Eq, Show)

data TouchEvent = Finger { finger :: !Index
                         , fstate :: !KeyState
                         , moving :: !Bool
                         , tpos :: !TouchPosVec -- *
                         , trel :: !TouchPosVec -- *
                         , pressure :: Float
                         }
                | Gesture { theta :: !Float
                          , dist :: !Float
                          , tpos :: !TouchPosVec -- *
                          , fingers :: !Word32
                          }
                | DollarGesture { gesture :: !Index
                                , fingers :: !Word32
                                , gerror :: !Float
                                , tpos :: !TouchPosVec -- *
                                }
                deriving (Eq, Show)

data JoystickType = IsJoystick | IsGameController
                  deriving (Eq, Show)

data EventData = Window !WindowID !WindowEvent
               | Joystick !JoyInstance !JoystickEvent
               | JoystickAdded !JoyID !JoystickType
               | Quit
               | SysWM !WMEvent
               | Touch !TouchID !TouchEvent
               | Drop { file :: !Text }
               deriving (Eq, Show)

data Event = Event !Ticks !EventData
           deriving (Show, Eq)
