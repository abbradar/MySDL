module Graphics.UI.SDL.Utils.Session
       ( SDLSession
       , sdlStep
       , Time
       , SDLStep
       , sdlSession
       ) where

import Data.Monoid (mempty)
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.Fixed (Milli, Fixed(..))

import Graphics.UI.SDL.Video.Monad
import Graphics.UI.SDL.Timer.Ticks
import Graphics.UI.SDL.Events.Types
import Graphics.UI.SDL.Events.Queue
import Graphics.UI.SDL.State.Types
import Graphics.UI.SDL.State

--   the only consumer of SDL events.
newtype SDLSession m = SDLSession { sdlStep :: SDLStep m }

type Time = Milli

type SDLStep m = forall a. ((Time, StateData) -> m a) -> m (a, SDLSession m)

-- | Create initial 'SDLSession'.
sdlSession :: forall m. (MonadSDLVideo m) => m (SDLSession m)
sdlSession =
  do
    t0 <- getTicks
    return $ SDLSession $ loop t0 [] emptyState

  where
    loop :: Ticks -> [EventData] -> StateData -> SDLStep m
    loop oldTime nextEvents state run = do
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

      r <- run (MkFixed dt, ss)

      return $ (r, SDLSession $ loop tf nes ss)
