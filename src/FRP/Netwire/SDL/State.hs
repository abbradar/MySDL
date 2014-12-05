{-|
Description: High-level SDL state.

Provides SDL state which can be advanced given recent queue of events.
-}
module FRP.Netwire.SDL.State
       ( HasSDLState(..)
       , State
       , nextState
       ) where

import Control.Monad (liftM, foldM)
import Control.Applicative ((<$>))
import Data.Monoid (Monoid, Last(..))
import qualified Data.Set as S
import qualified Data.BitSet.Word as BW
import qualified Data.Map.Strict as M
import Control.Lens
import Data.Maybe (fromJust)
import Control.Wire.Session (Timed(..))

import Graphics.UI.SDL
import Control.Lens.Instances ()
import FRP.Netwire.SDL.Types

-- | Instance to help construct user state for Netwire which
--   includes SDL state and then use provided wires with it.
class HasSDLState a where
  stateData :: a -> StateData

-- | Wrapper over StateData to provide 'mempty'.
newtype State = State (Last StateData)
              deriving (Show, Monoid)

instance HasSDLState State where
  stateData (State (Last (Just x))) = x
  stateData (State (Last Nothing)) = s
    where s = StateData { _rawEvents = []
                        , _windowState = M.empty
                        }

instance HasSDLState a => HasSDLState (a, b) where
  stateData = stateData . fst

instance HasSDLState a => HasSDLState (Timed t a) where
  stateData (Timed _ a) = stateData a

-- | Advance an SDL state.
--   During advancement it would query all necessary data to keep the state
--   consistent and up to date, given that all events that were received by
--   SDL were fed into it.
nextState :: forall m. MonadSDLVideo m => State -> [EventData] -> m State
nextState (stateData -> s0) es = State <$> Last <$> Just <$>
                                 foldM (flip upd) s0 { _rawEvents = reverse es } es
  where upd :: EventData -> StateData -> m StateData
        upd (Window i Shown) = \s -> (\r -> s & windowState.at i ?~ r) <$> def
          where def :: m WindowState
                def = do
                  -- May fail if there are no other references to Window.
                  Just w <- getWindowFromID i
                  _wpos <- getWindowPosition w
                  _wsize <- getWindowSize w
                  return WindowState { _keysPressed = S.empty
                                     , _scansPressed = S.empty
                                     , _modsPressed = BW.empty
                                     , _mouseState = M.empty
                                     , _wshown = True
                                     , _mouseFocus = False
                                     , _kbdFocus = False
                                     , ..
                                     }
        upd (Window i e) = windowState.at i %%~ liftM Just . winUpd e . fromJust
        upd e = return . case e of
          Window i Hidden -> rm i
          Window i Closed -> rm i
          _ -> id
          where rm i = windowState.at i .~ Nothing

        winUpd (Mouse i e) = mouseState.at i %%~ liftM (Just . mouseUpd e) . maybe def return
          where def = do
                  (_mousePos, _mousePressed) <- getRelativeMouseState
                  return MouseState { .. }
        winUpd e = return . case e of
          Moved wp -> wpos .~ wp
          SizeChanged ws -> wsize .~ ws
          WinEntered -> mouseFocus .~ True
          WinLeft -> mouseFocus .~ False
          FocusGained -> kbdFocus .~ True
          FocusLost -> kbdFocus .~ False
          Keyboard KeyboardEvent { _kstate, _keySym = KeySym { .. } } ->
            case _kstate of
             Pressed -> (keysPressed.at _keyCode ?~ ())
                        . (scansPressed.at _scanCode ?~ ())
                        . (modsPressed .~ _keyMod)
             Released -> (keysPressed.at _keyCode .~ Nothing)
                         . (scansPressed.at _scanCode .~ Nothing)
                         . (modsPressed .~ _keyMod)
          _ -> id
            
        mouseUpd (MMotion MouseMotionEvent { .. }) = (mousePos .~ _mmpos)
        mouseUpd (MButton MouseButtonEvent { .. }) =
          case _mstate of
               Pressed -> (mousePressed.at _mbutton ?~ ()) . (mousePos .~ _mbpos)
               Released -> (mousePressed.at _mbutton .~ Nothing) . (mousePos .~ _mbpos)
        mouseUpd _ = id
