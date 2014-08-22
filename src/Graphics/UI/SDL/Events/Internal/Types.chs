{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.Bits ((.&.))
import Data.Maybe (fromMaybe)
import qualified Data.BitSet.Word as B
import Graphics.Rendering.OpenGL.GL.Tensor (Vector2(..))
import Control.Exception (bracket)

import Data.Enum.Num
import Control.Monad.Maybe
import Data.Text.Foreign.Extra
import Graphics.UI.SDL.Internal.Prim (freeSDL)
import Graphics.UI.SDL.Timer.Internal (getAbsTicks)

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
  return $ Vector2 x y

ctextToEvent :: CEventType -> CEvent -> IO (Maybe WindowEvent)
ctextToEvent t e = runMaybeT $ do
  d <- MaybeT $ case t of
    SdlTextediting -> do
      tstart <- fromIntegral <$> {#get SDL_TextEditingEvent->start #} e
      tlength <- fromIntegral <$> {#get SDL_TextEditingEvent->length #} e

      return $ Just Editing { .. }
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
      v <- getVec fromIntegral {#get SDL_WindowEvent->data1 #} {#get SDL_WindowEvent->data2 #} ev
      return $ Moved v
    SdlWindoweventResized -> do
      v <- getVec fromIntegral {#get SDL_WindowEvent->data1 #} {#get SDL_WindowEvent->data2 #} ev
      return $ Resized v
    SdlWindoweventSizeChanged -> return SizeChanged
    SdlWindoweventMinimized -> return Minimized
    SdlWindoweventMaximized -> return Maximized
    SdlWindoweventRestored -> return Restored
    SdlWindoweventEnter -> return WinEntered
    SdlWindoweventLeave -> return WinLeft
    SdlWindoweventFocusGained -> return FocusGained
    SdlWindoweventFocusLost -> return FocusLost
    SdlWindoweventClose -> return Closed
    e -> fail $ "Unknown window event: " ++ show e

{#enum define CMouseMask { SDL_BUTTON_LMASK as CLeftMask
                         , SDL_BUTTON_MMASK as CMiddleMask
                         , SDL_BUTTON_RMASK as CRightMask
                         , SDL_BUTTON_X1MASK as CX1Mask
                         , SDL_BUTTON_X2MASK as CX2Mask
                         } deriving (Eq, Show) #}

cmouseToEvent :: CEventType -> CEvent -> IO (Maybe WindowEvent)
cmouseToEvent t e = runMaybeT $ do
  d <- MaybeT $ case t of
    SdlMousemotion -> do
      bf <- {#get SDL_MouseMotionEvent->state #} e
      mpos <- getVec fromIntegral {#get SDL_MouseMotionEvent->x #} {#get SDL_MouseMotionEvent->y #} e
      mrel <- getVec fromIntegral {#get SDL_MouseMotionEvent->xrel #} {#get SDL_MouseMotionEvent->yrel #} e

      let keys = [ (CLeftMask, MouseLeft)
                 , (CMiddleMask, MouseMiddle)
                 , (CRightMask, MouseRight)
                 , (CX1Mask, MouseX1)
                 , (CX2Mask, MouseX2)
                 ]
          mstates = B.fromList $ map snd $ filter (\(m, _) -> bf .&. m /= 0) $ map (\(a, b) -> (fromEnum' a, b)) keys

      return $ Just MMotion { .. }

    SdlMousebuttondown -> mousebtn Pressed
    SdlMousebuttonup -> mousebtn Released
    SdlMousewheel -> do
      mrel <- getVec fromIntegral {#get SDL_MouseWheelEvent->x #} {#get SDL_MouseWheelEvent->y #} e
      
      return $ Just MWheel { .. }

    _ -> return Nothing

  which <- lift $ fromIntegral <$> {#get SDL_MouseButtonEvent->which #} e

  return $ Mouse (if which == (-1) then MouseTouch else MouseID which) d

  where mousebtn mstate = do
          mbutton <- toEnum' <$> {#get SDL_MouseButtonEvent->button #} e
          clicks <- fromIntegral <$> {#get SDL_MouseButtonEvent->clicks #} e
          mpos <- getVec fromIntegral {#get SDL_MouseButtonEvent->x #} {#get SDL_MouseButtonEvent->y #} e

          return $ Just MButton { .. }

cwindowToEvent :: CEventType -> CEvent -> IO (Maybe EventData)
cwindowToEvent t e = runMaybeT $ do
  d <- MaybeT $ case t of
    SdlWindowevent -> Just <$> cwindowToData e
    SdlKeyup -> kbdbutton Pressed
    SdlKeydown -> kbdbutton Released
    _ -> sequenceMaybe $ map (\f -> f t e)
         [ ctextToEvent
         , cmouseToEvent
         ]
  
  wid <- lift $ fromIntegral <$> {#get SDL_WindowEvent->windowID #} e
  return $ Window wid d

  where kbdbutton kstate = do
          krepeat <- (\a -> if a /= 0 then True else False) <$> {#get SDL_KeyboardEvent->repeat #} e
          keySym <- fromCKeySym $ e `plusPtr` {#offsetof SDL_KeyboardEvent->keysym #}

          return $ Just Keyboard { .. }

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
      axis <- fromIntegral <$> {#get SDL_JoyAxisEvent->axis #} e
      japos <- fromIntegral <$> {#get SDL_JoyAxisEvent->value #} e

      return $ Just Axis { .. }
    SdlJoyballmotion -> do
      ball <- fromIntegral <$> {#get SDL_JoyBallEvent->ball #} e
      jbrel <- getVec fromIntegral {#get SDL_JoyBallEvent->xrel #} {#get SDL_JoyBallEvent->yrel #} e

      return $ Just Ball { .. }
    SdlJoyhatmotion -> do
      hat <- fromIntegral <$> {#get SDL_JoyHatEvent->hat #} e
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

      return $ Just Hat { .. }
    SdlJoybuttondown -> joybtn Pressed
    SdlJoybuttonup -> joybtn Released
    SdlJoydeviceremoved -> return $ Just Removed
    SdlControlleraxismotion -> do
      axis <- fromIntegral <$> {#get SDL_ControllerAxisEvent->axis #} e
      cpos <- fromIntegral <$> {#get SDL_ControllerAxisEvent->value #} e

      return $ Just ControllerAxis { .. }
    SdlControllerbuttondown -> ctrlbtn Pressed
    SdlControllerbuttonup -> ctrlbtn Released
    SdlControllerdeviceremoved -> return $ Just ControllerRemoved
    SdlControllerdeviceremapped -> return $ Just ControllerRemapped
    _ -> return Nothing

  jid <- lift $ fromIntegral <$> {#get SDL_JoyDeviceEvent->which #} e
  return $ Joystick jid d

  where joybtn jbstate = do
          jbutton <- fromIntegral <$> {#get SDL_JoyButtonEvent->button #} e

          return $ Just Button { .. }
        ctrlbtn cstate = do
          cbutton <- fromIntegral <$> {#get SDL_ControllerButtonEvent->button #} e

          return $ Just ControllerButton { .. }

ctouchToEvent :: CEventType -> CEvent -> IO (Maybe EventData)
ctouchToEvent t e = runMaybeT $ do
  d <- MaybeT $ case t of
    SdlFingermotion -> touch Pressed True
    SdlFingerdown -> touch Pressed False
    SdlFingerup -> touch Released False
    SdlMultigesture -> do
      theta <- convFloat <$> {#get SDL_MultiGestureEvent->dTheta #} e
      dist <- convFloat <$> {#get SDL_MultiGestureEvent->dDist #} e
      tpos <- getVec convFloat {#get SDL_MultiGestureEvent->x #} {#get SDL_MultiGestureEvent->y #} e
      fingers <- fromIntegral <$> {#get SDL_MultiGestureEvent->numFingers #} e
      
      return $ Just Gesture { .. }
    SdlDollargesture -> do
      gesture <- fromIntegral <$> {#get SDL_DollarGestureEvent->gestureId #} e
      fingers <- fromIntegral <$> {#get SDL_DollarGestureEvent->numFingers #} e
      gerror <- convFloat <$> {#get SDL_DollarGestureEvent->error #} e
      tpos <- getVec convFloat {#get SDL_DollarGestureEvent->x #} {#get SDL_DollarGestureEvent->y #} e

      return $ Just DollarGesture { .. }
    _ -> return Nothing

  tid <- lift $ fromIntegral <$> {#get SDL_TouchFingerEvent->touchId #} e
  return $ Touch tid d

  where touch fstate moving = do
          finger <- fromIntegral <$> {#get SDL_TouchFingerEvent->fingerId #} e
          tpos <- getVec convFloat {#get SDL_TouchFingerEvent->x #} {#get SDL_TouchFingerEvent->y #} e
          trel <- getVec convFloat {#get SDL_TouchFingerEvent->dx #} {#get SDL_TouchFingerEvent->dy #} e
          pressure <- convFloat <$> {#get SDL_TouchFingerEvent->pressure #} e
          
          return $ Just Finger { .. }

        convFloat (CFloat f) = f

#c

typedef struct SDL_SysWMmsg SDL_SysWMmsg_t;

#endc

--{#pointer *SDL_SysWMmsg_t as CSysWMMsg #}

csyswmToEvent :: CEvent -> IO EventData
csyswmToEvent e = do
  msg <- {#get SDL_SysWMEvent->msg #} e
  wm <- toEnum' <$> {#get SDL_SysWMmsg->subsystem #} e >>= \case
    SdlSyswmWindows -> return Windows
    SdlSyswmX11 -> return X11
    SdlSyswmDirectfb -> return DirectFB
    a -> error $ "Unknown window system: " ++ show a

  return $ SysWM wm

ceventToEvent :: CEvent -> IO Event
ceventToEvent ev = do
  ts <- fromIntegral <$> {#get SDL_CommonEvent->timestamp #} ev >>= getAbsTicks
  d <- toEnum' <$> {#get SDL_CommonEvent->type #} ev >>= \case
    SdlQuit -> return Quit
    SdlSyswmevent -> csyswmToEvent ev
    SdlDropfile -> do
      file <- bracket ({#get SDL_DropEvent->file #} ev) freeSDL peekCString

      return Drop { .. }
    e -> fromMaybe (error $ "Unsupported event type" ++ show e) <$> (sequenceMaybe $ map (\f -> f e ev)
         [ cwindowToEvent
         , cjoyToEvent
         , ctouchToEvent
         ])

  return $ Event ts d
