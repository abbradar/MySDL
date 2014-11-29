module Graphics.UI.SDL.Events.Internal.Types
       ( CEvent
       , ceventToEvent
       ) where

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.C.Types (CInt(..), CUInt(..), CFloat(..), CChar(..), CUShort(..), CUChar(..))
import Foreign.Storable (peekByteOff)
import Control.Applicative ((<$>))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Control.Exception (bracket)
import Text.Printf (printf)
import Data.Int
import Data.Word

import Graphics.UI.SDL.Types
import Data.Enum.Num
import Control.Monad.Maybe
import Data.Text.Foreign.Extra
import Graphics.UI.SDL.Internal.Prim (freeSDL)
import Graphics.UI.SDL.Video.Internal.Mouse
import Graphics.UI.SDL.Video.Internal.Window (WindowID(..))

{#import Graphics.UI.SDL.Events.Types #}
{#import Graphics.UI.SDL.Video.Internal.Keyboard #}
{#import Graphics.UI.SDL.Video.Internal.SysWM #}

#include <SDL2/SDL_events.h>
#include <SDL2/SDL_syswm.h>

{#pointer *SDL_Event as CEvent #}

{#enum SDL_EventType as CEventType {underscoreToCase}
    deriving (Eq, Show) #}

getVec :: (b -> c) -> (a -> IO b) -> (a -> IO b) -> a -> IO (Vector2 c)
getVec f f1 f2 e = do
  x <- f <$> f1 e
  y <- f <$> f2 e
  return $ P x y

fromCInt :: CInt -> Int32
fromCInt (CInt a) = a

fromCUInt :: CUInt -> Word32
fromCUInt (CUInt a) = a

fromCFloat :: CFloat -> Float
fromCFloat (CFloat a) = a

ctextToEvent :: CEventType -> CEvent -> IO (Maybe WindowEvent)
ctextToEvent t e = runMaybeT $ do
  d <- MaybeT $ case t of
    SdlTextediting -> do
      CInt _tstart <- {#get SDL_TextEditingEvent->start #} e
      CInt _tlength <- {#get SDL_TextEditingEvent->length #} e

      return $ Just $ Editing TextEditingEvent { .. }
    SdlTextinput -> return $ Just Input
    _ -> return Nothing
    
  text <- lift $ peekCString $ e `plusPtr` {#offsetof SDL_TextEditingEvent->text #}
  return $ Text text d

{#enum SDL_WindowEventID as CWindowEventType {underscoreToCase}
    deriving (Eq, Show) #}

cwindowToData :: CEvent -> IO WindowEvent
cwindowToData ev = toEnum' <$> {#get SDL_WindowEvent->event #} ev >>= \case
    SdlWindoweventShown -> return Shown
    SdlWindoweventHidden -> return Hidden
    SdlWindoweventExposed -> return Exposed
    SdlWindoweventMoved -> do
      v <- getVec fromCInt {#get SDL_WindowEvent->data1 #} {#get SDL_WindowEvent->data2 #} ev
      return $ Moved v
    SdlWindoweventResized -> do
      v <- getVec fromCInt {#get SDL_WindowEvent->data1 #} {#get SDL_WindowEvent->data2 #} ev
      return $ Resized v
    SdlWindoweventSizeChanged -> do
      v <- getVec fromCInt {#get SDL_WindowEvent->data1 #} {#get SDL_WindowEvent->data2 #} ev
      return $ SizeChanged v
    SdlWindoweventMinimized -> return Minimized
    SdlWindoweventMaximized -> return Maximized
    SdlWindoweventRestored -> return Restored
    SdlWindoweventEnter -> return WinEntered
    SdlWindoweventLeave -> return WinLeft
    SdlWindoweventFocusGained -> return FocusGained
    SdlWindoweventFocusLost -> return FocusLost
    SdlWindoweventClose -> return Closed
    e -> fail $ printf "Unknown window event: %s" $ show e

cmouseToEvent :: CEventType -> CEvent -> IO (Maybe WindowEvent)
cmouseToEvent t e = runMaybeT $ do
  d <- MaybeT $ case t of
    SdlMousemotion -> do
      mstates <- mmaskToButtons <$> fromCUInt <$> {#get SDL_MouseMotionEvent->state #} e
      mmpos <- getVec fromCInt {#get SDL_MouseMotionEvent->x #} {#get SDL_MouseMotionEvent->y #} e
      mrel <- getVec fromCInt {#get SDL_MouseMotionEvent->xrel #} {#get SDL_MouseMotionEvent->yrel #} e

      return $ Just $ MMotion MouseMotionEvent { .. }

    SdlMousebuttondown -> mousebtn Pressed
    SdlMousebuttonup -> mousebtn Released
    SdlMousewheel -> do
      mrel <- getVec fromCInt {#get SDL_MouseWheelEvent->x #} {#get SDL_MouseWheelEvent->y #} e
      
      return $ Just $ MWheel mrel

    _ -> return Nothing

  which <- lift $ {#get SDL_MouseButtonEvent->which #} e

  return $ Mouse (if which == (-1) then MouseTouch else MouseID which) d

  where mousebtn mstate = do
          mbutton <- toEnum' <$> {#get SDL_MouseButtonEvent->button #} e
          (CUChar clicks) <- {#get SDL_MouseButtonEvent->clicks #} e
          mbpos <- getVec fromCInt {#get SDL_MouseButtonEvent->x #} {#get SDL_MouseButtonEvent->y #} e

          return $ Just $ MButton MouseButtonEvent { .. }

cwindowToEvent :: CEventType -> CEvent -> IO (Maybe EventData)
cwindowToEvent t e = runMaybeT $ do
  d <- MaybeT $ case t of
    SdlWindowevent -> Just <$> cwindowToData e
    SdlKeydown -> kbdbutton Pressed
    SdlKeyup -> kbdbutton Released
    _ -> sequenceMaybe $ map (\f -> f t e)
         [ ctextToEvent
         , cmouseToEvent
         ]
  
  -- TODO: Sometimes SDL will send strange events with WindowID == 0. Let's ignore them for now.
  wid <- lift $ {#get SDL_WindowEvent->windowID #} e
  guard $ wid /= 0
  return $ Window (WindowID wid) d

  where kbdbutton _kstate = do
          _krepeat <- (\a -> if a /= 0 then True else False) <$> {#get SDL_KeyboardEvent->repeat #} e
          _keySym <- fromCKeySym $ e `plusPtr` {#offsetof SDL_KeyboardEvent->keysym #}

          return $ Just $ Keyboard KeyboardEvent { .. }

{#enum define CJoyHat { SDL_HAT_CENTERED as CCentered
                      , SDL_HAT_UP as CUp
                      , SDL_HAT_RIGHT as CRight
                      , SDL_HAT_DOWN as CDown
                      , SDL_HAT_LEFT as CLeft
                      , SDL_HAT_RIGHTUP as CRightUp
                      , SDL_HAT_RIGHTDOWN as CRightDown
                      , SDL_HAT_LEFTUP as CLeftUp
                      , SDL_HAT_LEFTDOWN as CLeftDown
                      } deriving (Eq, Show) #}

cjoyToEvent :: CEventType -> CEvent -> IO (Maybe EventData)
cjoyToEvent t e = runMaybeT $ do
  d <- MaybeT $ case t of
    SdlJoyaxismotion -> do
      axis <- {#get SDL_JoyAxisEvent->axis #} e
      CInt japos <- {#get SDL_JoyAxisEvent->value #} e

      return $ Just $ Axis axis japos
    SdlJoyballmotion -> do
      ball <- {#get SDL_JoyBallEvent->ball #} e
      jbrel <- getVec fromCInt {#get SDL_JoyBallEvent->xrel #} {#get SDL_JoyBallEvent->yrel #} e

      return $ Just $ Ball ball jbrel
    SdlJoyhatmotion -> do
      hat <- {#get SDL_JoyHatEvent->hat #} e
      v' <- {#get SDL_JoyHatEvent->value #} e
      let value = uncurry JoyHat $ case toEnum' v' of
            CCentered -> (XCenter, YCenter)
            CUp -> (XCenter, North)
            CRight -> (East, YCenter)
            CDown -> (XCenter, South)
            CLeft -> (West, YCenter)
            CRightUp -> (East, North)
            CRightDown -> (East, South)
            CLeftUp -> (West, North)
            CLeftDown -> (West, South)

      return $ Just $ Hat hat value
    SdlJoybuttondown -> joybtn Pressed
    SdlJoybuttonup -> joybtn Released
    SdlJoydeviceremoved -> return $ Just Removed
    SdlControlleraxismotion -> do
      axis <- {#get SDL_ControllerAxisEvent->axis #} e
      CInt cpos <- {#get SDL_ControllerAxisEvent->value #} e

      return $ Just $ ControllerAxis axis cpos
    SdlControllerbuttondown -> ctrlbtn Pressed
    SdlControllerbuttonup -> ctrlbtn Released
    SdlControllerdeviceremoved -> return $ Just ControllerRemoved
    SdlControllerdeviceremapped -> return $ Just ControllerRemapped
    _ -> return Nothing

  jid <- lift $ {#get SDL_JoyDeviceEvent->which #} e
  return $ Joystick jid d

  where joybtn jbstate = do
          jbutton <- {#get SDL_JoyButtonEvent->button #} e

          return $ Just $ Button jbutton jbstate
        ctrlbtn cstate = do
          cbutton <- {#get SDL_ControllerButtonEvent->button #} e

          return $ Just $ ControllerButton cbutton cstate

ctouchToEvent :: CEventType -> CEvent -> IO (Maybe EventData)
ctouchToEvent t e = runMaybeT $ do
  d <- MaybeT $ case t of
    SdlFingermotion -> touch Pressed True
    SdlFingerdown -> touch Pressed False
    SdlFingerup -> touch Released False
    SdlMultigesture -> do
      CFloat theta <- {#get SDL_MultiGestureEvent->dTheta #} e
      CFloat dist <- {#get SDL_MultiGestureEvent->dDist #} e
      tgpos <- getVec fromCFloat {#get SDL_MultiGestureEvent->x #} {#get SDL_MultiGestureEvent->y #} e
      CUShort gfingers <- {#get SDL_MultiGestureEvent->numFingers #} e
      
      return $ Just $ Gesture TouchGestureEvent { .. }
    SdlDollargesture -> do
      gesture <- {#get SDL_DollarGestureEvent->gestureId #} e
      CUInt dfingers <- {#get SDL_DollarGestureEvent->numFingers #} e
      gerror <- fromCFloat <$> {#get SDL_DollarGestureEvent->error #} e
      tdpos <- getVec fromCFloat {#get SDL_DollarGestureEvent->x #} {#get SDL_DollarGestureEvent->y #} e

      return $ Just $ DollarGesture TouchDollarEvent { .. }
    _ -> return Nothing

  tid <- lift $ {#get SDL_TouchFingerEvent->touchId #} e
  return $ Touch tid d

  where touch fstate moving = do
          finger <- {#get SDL_TouchFingerEvent->fingerId #} e
          tfpos <- getVec fromCFloat {#get SDL_TouchFingerEvent->x #} {#get SDL_TouchFingerEvent->y #} e
          trel <- getVec fromCFloat {#get SDL_TouchFingerEvent->dx #} {#get SDL_TouchFingerEvent->dy #} e
          CFloat pressure <- {#get SDL_TouchFingerEvent->pressure #} e
          
          return $ Just $ Finger TouchFingerEvent { .. }

#c

typedef struct SDL_SysWMmsg SDL_SysWMmsg_t;

#endc

--{#pointer *SDL_SysWMmsg_t as CSysWMMsg #}

-- TODO: Read platform-specific events
csyswmToEvent :: CEvent -> IO EventData
csyswmToEvent e = do
  --msg <- {#get SDL_SysWMEvent->msg #} e
  wm <- toEnum' <$> {#get SDL_SysWMmsg->subsystem #} e >>= \case
    SdlSyswmWindows -> return Windows
    SdlSyswmX11 -> return X11
    SdlSyswmDirectfb -> return DirectFB
    a -> fail $ printf "Unknown window system: %s" $ show a

  return $ SysWM wm

-- For complete safety from memory leaks, this should be called in masked environment.
ceventToEvent :: CEvent -> IO Event
ceventToEvent ev = do
  ts <- fromIntegral <$> {#get SDL_CommonEvent->timestamp #} ev
  d <- toEnum' <$> {#get SDL_CommonEvent->type #} ev >>= \case
    SdlQuit -> return Quit
    SdlSyswmevent -> csyswmToEvent ev
    SdlDropfile -> do
      file <- bracket ({#get SDL_DropEvent->file #} ev) freeSDL peekCString

      return $ Drop file
    e -> fromMaybe (error $ printf "Unsupported event type: %s" $ show e) <$> (sequenceMaybe $ map (\f -> f e ev)
         [ cwindowToEvent
         , cjoyToEvent
         , ctouchToEvent
         ])

  return $ Event ts d
