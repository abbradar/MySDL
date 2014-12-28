{-|
Description: Ticks for estimating time of events.
-}

module Graphics.UI.SDL.Timer.Ticks
       ( Ticks(..)
       , getTicks
       ) where

import Control.Applicative ((<$>))
import Foreign.C.Types (CUInt(..), CInt(..))
import Control.Monad.IO.ExClass


#include <SDL2/SDL_timer.h>

-- | Estimated ticks from SDL. They are meant only for relative comparison, as
--   they have no bounds and may overflow.
newtype Ticks = Ticks CInt
              deriving (Integral, Real, Num, Eq, Show)

instance Enum Ticks where
  -- Remove maxBound checks
  succ a = a + 1
  pred a = a - 1
  toEnum = Ticks . toEnum
  fromEnum (Ticks a) = fromEnum a

instance Ord Ticks where
  -- This is implemented as such in SDL_TICKS_PASSED
  (Ticks a) `compare` (Ticks b) = 0 `compare` (b - a)

-- | Get current number of ticks.
getTicks :: MonadIO' m => m Ticks
getTicks = liftIO $ fromIntegral <$> {#call unsafe SDL_GetTicks as ^ #}
