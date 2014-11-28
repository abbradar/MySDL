module Graphics.UI.SDL.Timer.Types
       ( Ticks(..)
       ) where

import Foreign.C.Types (CInt(..))

newtype Ticks = Ticks CInt
              deriving (Integral, Real, Num, Eq, Show)

instance Enum Ticks where
  -- Remove maxBound checks; Ticks are not Bounded.
  succ a = a + 1
  pred a = a - 1
  toEnum = Ticks . toEnum
  fromEnum (Ticks a) = fromEnum a

instance Ord Ticks where
  -- This is implemented as such in SDL_TICKS_PASSED
  (Ticks a) `compare` (Ticks b) = 0 `compare` (b - a)
