{-|
Description: Common types used by library.
-}
module Graphics.UI.SDL.Types
       ( V2(..)
       , Position
       , PosPoint
       , Size
       ) where

import Data.Int
import Linear.V2

-- For joystick positions and such
type Position = Int32

type PosPoint = V2 Position
type Size = V2 Int32
