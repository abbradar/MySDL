module FRP.Netwire.SDL
       ( SDLWire
       , SDLSession
       , sdlStep
       , sdlSession
       ) where

import Data.Monoid (mempty)
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Wire.Core (Wire, stepWire)
import Control.Wire.Session
import Data.Fixed (Milli, Fixed(..))

import Graphics.UI.SDL.Class
import Graphics.UI.SDL.Timer
import Graphics.UI.SDL.Timer.Types
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Events.Types
import FRP.Netwire.SDL.State

type SDLWire s = Wire (Timed Milli (State, s))

newtype SDLSession m = SDLSession { sdlStep :: SDLStep m }

type SDLStep m = forall s e a b. s -> SDLWire s e m a b -> Either e a ->
                 m (Either e b, SDLWire s e m a b, SDLSession m)

sdlSession :: forall m. (MonadSDLVideo m) => m (SDLSession m)
sdlSession =
  do
    t0 <- getTicks
    return $ SDLSession $ loop t0 [] mempty

  where
    loop :: Ticks -> [EventData] -> State -> SDLStep m
    loop oldTime nextEvents state s w' a = do
      pumpEvents
      tf <- getTicks

      let getEv = pollEvent >>= \case
            Nothing -> return ([], [])
            Just (Event t d)
              | t > tf -> return ([], [d])
              | otherwise -> first (d :) <$> getEv
    
      (es, nes) <- first (nextEvents ++) <$> getEv
      let !dt = fromIntegral $ tf - oldTime

      ss <- nextState state es

      (b, w) <- stepWire w' (Timed (MkFixed dt) (ss, s)) a

      return $ (b, w, SDLSession $ loop tf nes ss)
