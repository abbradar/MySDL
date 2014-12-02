{-|
Description: Receiving SDL events from the internal queue.
-}

module Graphics.UI.SDL.Events.Queue
       ( pollEvent
       , waitEvent
       , pumpEvents
       ) where

import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Control.Monad.Catch

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Events.Monad
import Graphics.UI.SDL.Events.Types

{#import Graphics.UI.SDL.Events.Internal.Types #}

#include <SDL2/SDL_events.h>

-- | Receive next event from internal SDL queue if there is any.
pollEvent :: MonadSDLEvents m => m (Maybe SDLEvent)
pollEvent = liftIO $ allocaBytesAligned {#sizeof SDL_Event #} {#alignof SDL_Event #} $
            -- SDL_PollEvent calls SDL_PumpEvents, which can call into Haskell via
            -- registered watches and filters, so this call should be "safe".
            \p -> mask_ $ sdlCall "SDL_PollEvent" ({#call SDL_PollEvent as ^#} p) (\x -> x == 0 || x == 1) >>=
                  \case
                    0 -> return Nothing
                    1 -> ceventToEvent p
                    _ -> undefined

-- | Wait for next SDL event to arrive.
waitEvent :: MonadSDLEvents m => m SDLEvent
-- It is implemented that way internally in SDL, and we re-implement
-- it here instead to avoid sleeping in foreign call.
waitEvent = pollEvent >>= \case
  Just x -> return x
  Nothing -> liftIO (threadDelay 10000) >> waitEvent

-- | Pump events from various sources into SDL's internal queue.
pumpEvents :: MonadSDLEvents m => m ()
pumpEvents = liftIO $ {#call SDL_PumpEvents as ^ #}
