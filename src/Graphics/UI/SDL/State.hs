{-|
Description: High-level SDL state.

Provides SDL state which can be advanced given recent queue of events.
-}
module Graphics.UI.SDL.State
       ( module Types
       , module Lens
       , module Advance
       ) where

import Graphics.UI.SDL.State.Types as Types
import Graphics.UI.SDL.State.Lens as Lens
import Graphics.UI.SDL.State.Advance as Advance
