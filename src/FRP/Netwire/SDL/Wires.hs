{-|
Description: Helpful wires for dealing with SDL events and state.
-}

module FRP.Netwire.SDL.Wires
       ( -- * Generic wires
         sdlOnState
       , sdlOnEvent
       , sdlOnEvent_
         -- * Keyboard
       , whileKey
       , onKey
         -- * Mouse
       , mouseMove
       ) where

import Prelude hiding ((.), id)
import Control.Wire hiding (First, at)
import Control.Wire.Unsafe.Event
import Control.Lens
import Control.Arrow
import Data.Int
import Data.Monoid (First)

import Graphics.UI.SDL
import Control.Lens.Extra
import FRP.Netwire.SDL.Types
import FRP.Netwire.SDL.State
import FRP.Netwire.SDL.Lens

type SDLWireC_ s e m = (HasSDLState s, Monoid s, Monad m)
type SDLWireC s e m = (SDLWireC_ s e m, Monoid e)
type SDLWireCE s e m = (SDLWireC_ s e m, Monoid e)

-- | Wire which emits a state if it satisfies given 'Traversable'.
sdlOnState :: SDLWireC s e m => Getting (First b) StateData b -> Wire s e m a b
sdlOnState t = mkPure $ \(stateData -> s) _ -> (maybe (Left mempty) Right $ s ^? t, sdlOnState t)

-- | Wire which emits an event when it satisfies given 'Traversable'.
sdlOnEvent :: (SDLWireCE s e m) => Getting (Endo [b]) EventData b -> Wire s e m a (UniqueEvent [b])
sdlOnEvent t = mkSF $ \(stateData -> StateData { _rawEvents }) _ ->
                       (fromEvent $ check $ _rawEvents ^.. traversed . t, sdlOnEvent t)
  where check [] = NoEvent
        check x = Event x

-- | Same as 'sdlOnEvent', but emits only the last occurred event.
sdlOnEvent_ :: (SDLWireCE s e m) => Getting (Endo [b]) EventData b -> Wire s e m a (UniqueEvent b)
sdlOnEvent_ t = arr (fmap head) . sdlOnEvent t

-- | Wire which emits while certain key is down in any window.
whileKey :: SDLWireC s e m => KeyState -> KeyCode -> Wire s e m a WindowState
whileKey s k = sdlOnState $ anyWindowState . (if s == Pressed then hasInside l else hasn'tInside l)
  where l :: Applicative f => (() -> f ()) -> WindowState -> f WindowState
        l = keysPressed . ix k

-- | Wire which emits a key when it is pressed in any window.
onKey :: (SDLWireCE s e m) => KeyState -> KeyCode -> Wire s e m a (UniqueEvent [KeyboardEvent])
onKey state key = sdlOnEvent $ anyWindow . _Keyboard . eqInside kstate state . eqInside (keySym . keyCode) key

-- | Emits relative mouse movement.
mouseMove :: SDLWireC s e m => Maybe WhichMouse -> Wire s e m a (Int32, Int32)
mouseMove m = arr (toTuple2 . foldr1 (+)) . onU . sdlOnEvent path <|> pure (0, 0)
  where path = anyWindow . _Mouse . maybe id (eqInside _1) m . _2 . _MMotion . mrel
