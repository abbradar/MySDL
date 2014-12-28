module Graphics.UI.SDL.Utils.Session
       ( SDLSession
       , Time
       , newSDLSession
       , sdlStep
       ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad.IO.ExClass
import Data.Fixed (Milli, Fixed(..))
import Data.IORef

import Graphics.UI.SDL.Timer.Ticks
import Graphics.UI.SDL.Events.Types
import Graphics.UI.SDL.Events.Queue
import Graphics.UI.SDL.State.Types
import Graphics.UI.SDL.State

-- | Session which provides consistent events fetching and elapsed time.
--   counter.
newtype SDLSession = SDLSession (IORef (Ticks, [EventData], StateData))

-- | Time type, with SDL timers' precision (milliseconds).
type Time = Milli

-- | Create initial 'SDLSession'.
newSDLSession :: MonadIO' m => m SDLSession
newSDLSession = do
  t0 <- getTicks
  liftIO $ SDLSession <$> newIORef (t0, [], emptyState)

-- | Fetch new events, advance time and update the state.
sdlStep :: MonadIO' m => SDLSession -> m (Time, StateData)
sdlStep (SDLSession st) = do
  (oldTime, nextEvents, state) <- liftIO $ readIORef st
  pumpEvents
  tf <- getTicks

  let getEv = pollEvent >>= \case
        Nothing -> return ([], [])
        Just (SDLEvent t d)
          | t > tf -> return ([], [d])
          | otherwise -> first (d :) <$> getEv
    
  (es, nes) <- first (nextEvents ++) <$> getEv
  let !dt = fromIntegral $ tf - oldTime
  ss <- nextState state es

  liftIO $ writeIORef st (tf, nes, ss)
  return (MkFixed dt, ss)
