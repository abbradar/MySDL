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
import Control.Lens
import Data.Monoid (First)

import Graphics.UI.SDL
import Control.Lens.Extra
import FRP.Netwire.SDL.Types
import FRP.Netwire.SDL.State
import FRP.Netwire.SDL.Lens

-- | Wire which emits a state if it satisfies given 'Traversable'.
sdlOnState :: (HasSDLState s, Monoid s, Monoid e, Monad m) => Getting (First b) StateData b -> Wire s e m a b
sdlOnState t = mkPure $ \(stateData -> s) _ -> (maybe (Left mempty) Right $ s ^? t, sdlOnState t)

-- | Wire which emits an SDL event when it satisfies given 'Traversable'.
--   We don't use Netwire 'Event' because SDL and our running semantics
--   guarantee that each time a wire produces a value, it would be an
--   unique event.
sdlOnEvent :: (HasSDLState s, Monoid s, Monad m) => Getting (First b) EventData b -> Wire s e m a b
sdlOnEvent t = mkPure $ \(stateData -> StateData { _rawEvents }) _ ->
                         ( maybe (Left mempty) Right $ _rawEvents ^? traversed . t, sdlOnEvent t)

-- | Wire which emits while certain key is down in any window.
whileKey :: (HasSDLState s, Monoid s, Monoid e, Monad m) => KeyState -> KeyCode -> Wire s e m a WindowState
whileKey s k = sdlOnState $ anyWindowState . (if s == Pressed then hasInside l else hasn'tInside l)
  where l :: Applicative f => (() -> f ()) -> WindowState -> f WindowState
        l = keysPressed . ix k

-- | Wire which produces an event once when key is pressed in any window.
onKey :: (HasSDLState s, Monoid s, Monad m) => KeyState -> KeyCode -> Wire s e m a KeyboardEvent
onKey state key = sdlOnEvent $ anyWindow . _Keyboard . eqInside kstate state . eqInside (keySym . keyCode) key
