{-|
Description: SDL core monad transformer.
-}

{-# LANGUAGE UndecidableInstances #-}

module Graphics.UI.SDL.Monad
       ( SDLT
       , MonadSDL
       , withSDL
       ) where

import Control.Monad
import Control.Concurrent
import Foreign.C.Types (CInt(..), CUInt(..))
import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Catch

import Graphics.UI.SDL.Internal.Class

{#import Graphics.UI.SDL.Internal.Prim #}

#include <SDL2/SDL.h>

-- | SDL core monad transformer.
newtype SDLT m a = SDLT { runSDLT :: m a }
                 deriving ( Functor, Applicative, Monad
                          , MonadFix, MonadIO, MonadThrow
                          , MonadCatch, MonadMask
                          )

instance MonadTrans SDLT where
  lift = SDLT

instance (Applicative m, MonadIO m, MonadMask m) => MonadSDL (SDLT m) where

-- | Run with initialized core SDL.
withSDL :: (Applicative m, MonadIO m, MonadMask m) => SDLT m a -> m a
withSDL = unsafeWithSubSystem
          (liftIO $ do
              bound <- isCurrentThreadBound
              unless bound $ fail "SDL calls must be done in a bound thread"
              sdlCode "SDL_Init" $ sDLInit mainSystem
          )
          (liftIO {#call unsafe SDL_Quit as ^ #})
          mainSystem . runSDLT
  where mainSystem = NoParachute
        {#fun unsafe SDL_Init as ^ { `SDLSubSystem' } -> `Int' #}
