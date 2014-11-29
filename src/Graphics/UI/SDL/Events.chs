module Graphics.UI.SDL.Events
       ( pollEvent
       , unsafePollEvent
       , waitEvent
       , unsafeWaitEvent
       , pumpEvents
       , unsafePumpEvents
       ) where

import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Control.Monad.Base (liftBase)
import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (mask_)

import Graphics.UI.SDL.Class
import Graphics.UI.SDL.Events.Types

{#import Graphics.UI.SDL.Events.Internal.Types #}

#include <SDL2/SDL_events.h>

pollEvent' :: MonadSDLEvents m => (CEvent -> IO CInt) -> m (Maybe Event)
pollEvent' call = liftBase $ allocaBytesAligned {#sizeof SDL_Event #} {#alignof SDL_Event #} $ \p -> mask_ $ do
                    call p >>= \case
                      0 -> return Nothing
                      1 -> ceventToEvent p
                      _ -> fail "SDL_PollEvent: Unknown return code"

pollEvent :: MonadSDLEvents m => m (Maybe Event)
-- SDL_PollEvent calls SDL_PumpEvents, which can call into Haskell via
-- registered watches and filters, so this call should be "safe".
pollEvent = pollEvent' {#call SDL_PollEvent as sDLPollEvent #}

unsafePollEvent :: MonadSDLEvents m => m (Maybe Event)
unsafePollEvent = pollEvent' {#call unsafe SDL_PollEvent as sDLPollEventUnsafe #}

-- It is implemented that way internally in SDL, and we re-implement
-- it here instead to avoid sleeping in foreign call.
waitEvent' :: MonadSDLEvents m => m (Maybe Event) -> m Event
waitEvent' poll = poll >>= \case
  Just x -> return x
  Nothing -> liftBase (threadDelay 10000) >> waitEvent' poll

waitEvent :: MonadSDLEvents m => m Event
waitEvent = waitEvent' pollEvent

unsafeWaitEvent :: MonadSDLEvents m => m Event
unsafeWaitEvent = waitEvent' unsafePollEvent

pumpEvents :: MonadSDLEvents m => m ()
pumpEvents = liftBase $ {#call SDL_PumpEvents as sDLPumpEvents #}

unsafePumpEvents :: MonadSDLEvents m => m ()
unsafePumpEvents = liftBase $ {#call unsafe SDL_PumpEvents as sDLPumpEventsUnsafe #}
