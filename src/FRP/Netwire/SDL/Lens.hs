{-|
Description: Helpful lens for SDL state.
-}
module FRP.Netwire.SDL.Lens where

-- Splitted from Types because of TH

import Control.Applicative
import Control.Lens

import FRP.Netwire.SDL.Types

-- | A 'Traversable' into all available windows in state. Useful when you
--   have only one window (the most popular case).
anyWindowState :: (Indexable Int p, Applicative f) =>
                  p WindowState (f WindowState) -> StateData -> f StateData
anyWindowState = windowState . traversed

anyMouseState :: (Indexable Int p, Applicative f) =>
                  p MouseState (f MouseState) -> WindowState -> f WindowState
anyMouseState = mouseState . traversed
