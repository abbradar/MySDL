{-|
Description: Helpful wires for dealing with SDL events and state.
-}

module FRP.Netwire.SDL.Wires
       ( -- * Generic wires
         sdlOnEvent
       , sdlOnState
         -- * Keyboard-related
       , whileKey
       , onKey
       ) where

import Prelude hiding ((.))
import Control.Wire hiding (First, at)
import Control.Wire.Unsafe.Event
import Control.Lens
import Data.Monoid (First)

import Graphics.UI.SDL
import Control.Lens.Extra
import FRP.Netwire.SDL.Types
import FRP.Netwire.SDL.State
import FRP.Netwire.SDL.Lens

type SDLWireC_ s e m = (HasSDLState s, Monoid s, Monad m)
type SDLWireC s e m = (SDLWireC_ s e m, Monoid e)

deeventize :: SDLWireC s e m => Wire s e m (Event a) a
deeventize = mkPure_ $ \case
  NoEvent -> Left mempty
  Event a -> Right a

-- | Wire which emits a state if it satisfies given 'Traversable'.
sdlOnState :: SDLWireC s e m => Getting (First b) StateData b -> Wire s e m a b
sdlOnState t = mkPure $ \(stateData -> s) _ -> (maybe (Left mempty) Right $ s ^? t, sdlOnState t)

-- | As 'sdlOnEvent', but produces a netwire event each time SDL event occurs.
sdlOnEvent_ :: SDLWireC_ s e m => Getting (First b) EventData b -> Wire s e m a (Event b)
sdlOnEvent_ t = mkSF $ \(stateData -> StateData { _rawEvents }) _ ->
                        ( maybe NoEvent Event $ _rawEvents ^? traversed . t, sdlOnEvent_ t)

-- | Wire which emits an SDL event when it satisfies given 'Traversable'.
--   We not require using Netwire's 'Event' because SDL and our running semantics
--   guarantee that each time a wire produces a value, it would be an
--   unique event.
sdlOnEvent :: SDLWireC s e m => Getting (First b) EventData b -> Wire s e m a b
sdlOnEvent t = deeventize . sdlOnEvent_ t

-- | Wire which emits while certain key is down in any window.
whileKey :: SDLWireC s e m => KeyState -> KeyCode -> Wire s e m a WindowState
whileKey s k = sdlOnState $ anyWindowState . (if s == Pressed then hasInside l else hasn'tInside l)
  where l :: Applicative f => (() -> f ()) -> WindowState -> f WindowState
        l = keysPressed . ix k

-- | As 'onKey', but produces a netwire event each time SDL event occurs.
onKey_ :: SDLWireC_ s e m => KeyState -> KeyCode -> Wire s e m a (Event KeyboardEvent)
onKey_ state key = sdlOnEvent_ $ anyWindow . _Keyboard . eqInside kstate state . eqInside (keySym . keyCode) key

-- | Wire which emits a key when it is pressed in any window.
onKey :: SDLWireC s e m => KeyState -> KeyCode -> Wire s e m a KeyboardEvent
onKey s k = deeventize . onKey_ s k
