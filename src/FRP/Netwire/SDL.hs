{-# LANGUAGE BangPatterns #-}

module FRP.Netwire.SDL
       ( sdlSession
       , sdlStep
       ) where

import Control.Wire.Session
import Control.Monad.Loops (unfoldM)

import Graphics.UI.SDL.Class
import Graphics.UI.SDL.Timer
import Graphics.UI.SDL.Timer.Types
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Utils.Framerate

-- Actually, maybe we can use just Int as Timed type -- then
-- maybe it is even unnecesarry to have absolute time value
-- (in proper Integers).
sdlSession :: MonadSDL m => Session m (s -> Timed Ticks s)
sdlSession =
  Session $ do
    t0 <- getTicks
    return (Timed 0, loop t0)

  where
    loop t' = Session $ do
      t <- getTicks
      let !dt = fromIntegral $ t - t'
      return (Timed dt, loop t)

sdlStep :: MonadSDLEvents m => Session m (s -> Timed Ticks s) -> s -> m ()
sdlStep session s = do
  evs <- unfoldM pollEvent
  clock <- getTicks
  return ()
