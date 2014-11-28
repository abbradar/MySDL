module Graphics.UI.SDL.Utils.Framerate
       ( FPSLimit
       , runFPSLimit
       , limitFPS
       ) where

import Data.Int
import Control.Concurrent (threadDelay)
import Control.Monad.Base (liftBase)

import Graphics.UI.SDL.Timer
import Graphics.UI.SDL.Class

newtype FPSLimit m = FPSLimit { runFPSLimit :: (Int32 -> m (Int32, FPSLimit m)) }

limitFPS :: MonadSDL m => m (FPSLimit m)
limitFPS = getTicks >>= return . FPSLimit . loop
  where
    loop old limit = do
      let target = old + fromIntegral limit
      curr <- getTicks
      next <- liftBase $ if target > curr
                         then do
                           threadDelay $ 1000 * fromIntegral (target - curr)
                           return target
                         else return curr
      return (fromIntegral $ curr - old, FPSLimit $ loop next)
