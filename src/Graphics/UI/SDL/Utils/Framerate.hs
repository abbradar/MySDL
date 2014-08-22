{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.SDL.Utils.Framerate
       ( FPSLimiter
       , newFPSLimiter
       , limitFPS
       , limitFPS_
       ) where

import Data.Int
import Control.Applicative ((<$>))
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.Base (liftBase)

import Graphics.UI.SDL.Timer
import Graphics.UI.SDL.Class

newtype FPSLimiter = FPSLimiter (IORef Int32)

newFPSLimiter :: MonadSDL m => m FPSLimiter
newFPSLimiter = do
  t0 <- fromIntegral <$> getTicks
  liftBase $ FPSLimiter <$> newIORef t0

limitFPS :: MonadSDL m => FPSLimiter -> Int32 -> m Int32
limitFPS (FPSLimiter f) limit = do
  old <- liftBase $ readIORef f
  let target = old + limit
  curr <- fromIntegral <$> getTicks
  liftBase $ if target > curr
    then do
      threadDelay $ 1000 * fromIntegral (curr - target)
      writeIORef f target
    else writeIORef f curr
  return $ curr - old

limitFPS_ :: MonadSDL m => FPSLimiter -> Int32 -> m ()
limitFPS_ f l = limitFPS f l >> return ()
