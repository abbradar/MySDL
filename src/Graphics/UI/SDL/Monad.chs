{-# LANGUAGE UndecidableInstances #-}

module Graphics.UI.SDL.Monad
       ( SDLT
       , withSDL
       ) where

import Foreign.C.Types (CInt(..), CUInt(..))
import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))

import Graphics.UI.SDL.Class

{#import Graphics.UI.SDL.Internal.Prim #}

#include <SDL2/SDL.h>

-- Main SDL monad transformer.
newtype SDLT m a = SDLT { runSDLT :: m a }
                 deriving (Functor, Applicative, Monad,
                           MonadFix)

instance MonadTrans SDLT where
  lift = SDLT

deriving instance MonadBase IO m => MonadBase IO (SDLT m)
deriving instance MonadIO m => MonadIO (SDLT m)

instance MonadBaseControl IO m => MonadBaseControl IO (SDLT m) where
  newtype StM (SDLT m) a = StM {unStM :: StM m a}
  liftBaseWith = liftBaseThreaded SDLT runSDLT withSDL StM
  restoreM = SDLT . restoreM . unStM

instance MonadBase IO m => MonadSDL (SDLT m) where

withSDL :: (MonadBaseControl IO m, MonadBase IO m) => SDLT m a -> m a
withSDL = unsafeWithSubSystem
          (liftBase $ sdlCode "SDL_Init" $ sDLInit mainSystem)
          (liftBase {#call unsafe SDL_Quit as ^ #})
          mainSystem . runSDLT
  where mainSystem = NoParachute
        -- TODO: Change 'Int' to 'SDLSubSystem' when issue #103 in c2hs is fixed.
        {#fun unsafe SDL_Init as ^ { `SDLSubSystem' } -> `Int' #}
