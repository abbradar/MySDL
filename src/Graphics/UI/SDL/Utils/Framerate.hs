module Graphics.UI.SDL.Utils.Framerate
       ( FPSLimit
       , fpsLimit
       , fpsSession
       ) where

import Data.Int
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class

import Graphics.UI.SDL.Timer
import Graphics.UI.SDL.Class

newtype FPSLimit m = FPSLimit { fpsLimit :: (Int32 -> m (Int32, FPSLimit m)) }

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
