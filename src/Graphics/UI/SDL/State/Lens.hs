{-|
Description: Helpful lens for SDL state.
-}
module Graphics.UI.SDL.State.Lens where

-- Splitted from Types because of TH

import Control.Applicative
import Control.Lens

import Graphics.UI.SDL.State.Types

-- | A 'Traversable' into all available windows in state. Useful when you
--   have only one window (the most popular case).
anyWindowState :: (Indexable Int p, Applicative f) =>
                  p WindowState (f WindowState) -> StateData -> f StateData
anyWindowState = windowState . traversed

-- | A 'Traversable' into all available mouses in state.
anyMouseState :: (Indexable Int p, Applicative f) =>
                  p MouseState (f MouseState) -> WindowState -> f WindowState
anyMouseState = mouseState . traversed
