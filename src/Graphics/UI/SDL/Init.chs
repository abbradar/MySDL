{-|
Description: SDL initialization.
-}

{-# LANGUAGE UndecidableInstances #-}

module Graphics.UI.SDL.Init
       ( SDLSubSystem( SDLTimer
                     , SDLAudio
                     , SDLVideo
                     , SDLJoystick
                     , SDLHaptic
                     , SDLGameController
                     , SDLEvents
                     )
       , withSDL
       ) where

import Control.Monad
import Control.Concurrent
import Data.Bits ((.|.))
import Foreign.C.Types (CInt(..), CUInt(..))
import Control.Monad.IO.ExClass
import Control.Monad.Catch

import Data.Enum.Num
import Graphics.UI.SDL.Internal.Prim

#define SDL_MAIN_HANDLED
#include <SDL2/SDL.h>

{#enum define SDLSubSystem { SDL_INIT_TIMER as SDLTimer
                           , SDL_INIT_AUDIO as SDLAudio
                           , SDL_INIT_VIDEO as SDLVideo
                           , SDL_INIT_JOYSTICK as SDLJoystick
                           , SDL_INIT_HAPTIC as SDLHaptic
                           , SDL_INIT_GAMECONTROLLER as SDLGameController
                           , SDL_INIT_EVENTS as SDLEvents
                           , SDL_INIT_NOPARACHUTE as NoParachute
                           } deriving (Show, Eq, Ord) #}

-- | Run with initialized core SDL.
withSDL :: (MonadIO' m, MonadMask m) => [SDLSubSystem] -> m a -> m a
withSDL ss = bracket_
             (liftIO $ do
                 bound <- isCurrentThreadBound
                 unless bound $ fail "SDL calls must be done in a bound thread"
                 let f = foldr ((.|.) . fromEnum') 0 $ NoParachute : ss
                 sdlCode "SDL_Init" $ sDLInit f
             )
             (liftIO {#call unsafe SDL_Quit as ^ #})
  where {#fun unsafe SDL_Init as ^ { `Int' } -> `Int' #}
