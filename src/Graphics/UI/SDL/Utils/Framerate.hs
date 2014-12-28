{-|
Description: Frames-per-second limiter based on SDL timer subsystem.
-}

module Graphics.UI.SDL.Utils.Framerate
       ( FPSLimit
       , newFPSLimit
       , fpsLimit
       ) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.ExClass
import Control.Applicative
import Data.IORef

import Graphics.UI.SDL.Timer

-- | FPS limiter using SDL timer.
newtype FPSLimit = FPSLimit (IORef Ticks)

-- | Create initial 'FPSLimit'.
newFPSLimit :: MonadIO' m => m FPSLimit
newFPSLimit = do
  t0 <- getTicks
  liftIO $ FPSLimit <$> newIORef t0

-- | Given frame time (inverted FPS), delay a thread.
fpsLimit :: MonadIO' m => FPSLimit -> Ticks -> m Ticks
fpsLimit (FPSLimit st) limit = do
  old <- liftIO $ readIORef st
  let target = old + fromIntegral limit
  curr <- getTicks
  next <- liftIO $ if target > curr
                   then do
                     threadDelay $ 1000 * fromIntegral (target - curr)
                     return target
                   else return curr
  liftIO $ writeIORef st next
  return $ fromIntegral $ curr - old
