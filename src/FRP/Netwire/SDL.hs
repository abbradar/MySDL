{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module FRP.Netwire.SDL
       ( sdlSession
       , InternalState
       , newInternalState
       , SDLWire
       , sdlStep
       ) where

import Data.Monoid (mempty)
import Control.Monad.Base (liftBase)
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Wire.Core (Wire, stepWire)
import Control.Wire.Session
import Data.IORef
import Data.Fixed (Milli, Fixed(..))

import Graphics.UI.SDL.Class
import Graphics.UI.SDL.Timer
import Graphics.UI.SDL.Timer.Types
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Events.Types
import FRP.Netwire.SDL.State

sdlSession :: MonadSDL m => Session m (s -> Timed Integer s)
sdlSession =
  Session $ do
    t0 <- getTicks
    return (Timed 0, loop t0)

  where
    loop t' = Session $ do
      t <- getTicks
      let !dt = fromIntegral $ t - t'
      return (Timed dt, loop t)

data IState = IState { oldTime :: Ticks
                     , nextEvents :: [EventData]
                     , state :: State
                     }

newtype InternalState = InternalState (IORef IState)

newInternalState :: MonadSDL m => m InternalState
newInternalState = do
  t0 <- getTicks
  liftBase $ InternalState <$> newIORef IState { oldTime = t0
                                               , nextEvents = []
                                               , state = mempty
                                               }

type SDLWire s = Wire (Timed Milli (State, s))

sdlStep :: MonadSDLVideo m => InternalState -> s -> SDLWire s e m a b -> Either e a -> m (Either e b, SDLWire s e m a b)
sdlStep (InternalState is') s w i = do
  is <- liftBase $ readIORef is'
  pumpEvents
  tf <- getTicks

  let getEv = pollEvent >>= \case
        Nothing -> return ([], [])
        Just (Event t d)
          | t > tf -> return ([], [d])
          | otherwise -> first (d :) <$> getEv
    
  (es, nes) <- first (nextEvents is ++) <$> getEv
  let !dt = MkFixed $ fromIntegral $ tf - oldTime is

  ss <- nextState (state is) es

  liftBase $ writeIORef is' IState { oldTime = tf
                                   , nextEvents = nes
                                   , state = ss
                                   }
  stepWire w (Timed dt (ss, s)) i
