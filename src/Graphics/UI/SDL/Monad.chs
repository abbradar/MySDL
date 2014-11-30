{-# LANGUAGE UndecidableInstances #-}

module Graphics.UI.SDL.Monad
       ( SDLT
       , withSDL
       ) where

import Foreign.C.Types (CInt(..), CUInt(..))
import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Catch

import Graphics.UI.SDL.Class

{#import Graphics.UI.SDL.Internal.Prim #}

#include <SDL2/SDL.h>

-- Main SDL monad transformer.
newtype SDLT m a = SDLT { runSDLT :: m a }
                 deriving ( Functor, Applicative, Monad
                          , MonadFix, MonadIO, MonadThrow
                          , MonadCatch, MonadMask
                          )

instance MonadTrans SDLT where
  lift = SDLT

instance (Applicative m, MonadIO m, MonadMask m) => MonadSDL (SDLT m) where

withSDL :: (Applicative m, MonadIO m, MonadMask m) => SDLT m a -> m a
withSDL = unsafeWithSubSystem
          (liftIO $ sdlCode "SDL_Init" $ sDLInit mainSystem)
          (liftIO {#call unsafe SDL_Quit as ^ #})
          mainSystem . runSDLT
  where mainSystem = NoParachute
        {#fun unsafe SDL_Init as ^ { `SDLSubSystem' } -> `Int' #}
