module FRP.Netwire.SDL.Wires where

import Prelude hiding ((.))
import Control.Wire hiding (First, at)
import Control.Wire.Unsafe.Event (Event(..))
import Control.Lens
import Data.Monoid (First)

import Control.Lens.Extra
import FRP.Netwire.SDL.Types
import FRP.Netwire.SDL.State
import Graphics.UI.SDL.Video.Keyboard.Types
import Graphics.UI.SDL.Events.Types hiding (Event)

mapE :: (a -> b) -> Wire s e m (Event a) (Event b)
mapE f = mkSF_ $ \case
  NoEvent -> NoEvent
  Event a -> Event $ f a

emptyE :: Wire s e m (Event a) (Event ())
emptyE = mapE $ const ()

sdlOnEvent :: (HasSDLState s, Monoid s, Monad m) => Getting (First b) EventData b -> Wire s e m a (Event b)
sdlOnEvent t = mkSF $ \(stateData -> StateData { _rawEvents }) _ ->
                       ( maybe NoEvent Event $ _rawEvents ^? traversed . t
                       , sdlOnEvent t)

sdlOnState :: (HasSDLState s, Monoid s, Monoid e, Monad m) => Getting (First b) StateData b -> Wire s e m a b
sdlOnState t = mkPure $ \(stateData -> s) _ -> (maybe (Left mempty) Right $ s ^? t, sdlOnState t)

-- Helpful wires

whileKey :: (HasSDLState s, Monoid s, Monoid e, Monad m) => KeyState -> KeyCode -> Wire s e m a WindowState
whileKey s k = sdlOnState $ windowState . traversed . (if s == Pressed then hasInside l else hasn'tInside l)
  where l = keysPressed . ix k

onKey :: (HasSDLState s, Monoid s, Monad m) => KeyState -> KeyCode -> Wire s e m a (Event KeyboardEvent)
onKey state key = sdlOnEvent $ _Window . _2 . _Keyboard . eqInside kstate state . eqInside (keySym . keyCode) key
