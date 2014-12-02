{-|
Description: Frames-per-second limiter based on SDL timer subsystem.
-}

module Graphics.UI.SDL.Utils.Framerate
       ( FPSLimit
       , fpsLimit
       , fpsSession
       ) where

import Data.Int
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class

import Graphics.UI.SDL.Timer
import Graphics.UI.SDL.Monad

-- | Mini-wire which delays thread to limit frames per second.
--
-- [@fpsLimit@] Given frame time (inverted FPS), delay a thread and advance internal state.
newtype FPSLimit m = FPSLimit { fpsLimit :: (Int32 -> m (Int32, FPSLimit m)) }

-- | Create initial 'FPSLimit'.
fpsSession :: MonadSDL m => m (FPSLimit m)
fpsSession = getTicks >>= return . FPSLimit . loop
  where
    loop old limit = do
      let target = old + fromIntegral limit
      curr <- getTicks
      next <- liftIO $ if target > curr
                         then do
                           threadDelay $ 1000 * fromIntegral (target - curr)
                           return target
                         else return curr
      return (fromIntegral $ curr - old, FPSLimit $ loop next)
