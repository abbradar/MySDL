{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.SDL.Utils.Framerate
       ( FPSLimiter
       , newFPSLimiter
       , limitFPS
       , limitFPS_
       ) where

import Data.Int
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.Base (liftBase)
import Control.Applicative ((<$>))

import Graphics.UI.SDL.Timer
import Graphics.UI.SDL.Timer.Types
import Graphics.UI.SDL.Class

newtype FPSLimiter = FPSLimiter (IORef Ticks)

newFPSLimiter :: MonadSDL m => m FPSLimiter
newFPSLimiter = do
  t0 <- getTicks
  liftBase $ FPSLimiter <$> newIORef t0

limitFPS :: MonadSDL m => FPSLimiter -> Int32 -> m Int32
limitFPS (FPSLimiter f) limit = do
  old <- liftBase $ readIORef f
  let target = old + fromIntegral limit
  curr <- getTicks
  liftBase $ if target > curr
    then do
      threadDelay $ 1000 * fromIntegral (target - curr)
      writeIORef f target
    else writeIORef f curr
  return $ fromIntegral $ curr - old

limitFPS_ :: MonadSDL m => FPSLimiter -> Int32 -> m ()
limitFPS_ f l = limitFPS f l >> return ()
