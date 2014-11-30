module Graphics.UI.SDL.Video.Monad
       ( SDLVideoT
       , MonadSDLVideo
       , withSDLVideo
       ) where

import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Catch

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Class

-- SDL Video monad transformer
newtype SDLVideoT m a = SDLVideoT { runSDLVideoT :: m a }
              deriving ( Functor, Applicative, Monad
                       , MonadFix, MonadIO, MonadThrow
                       , MonadCatch, MonadMask

                       , MonadSDL, MonadSDLAudio
                       , MonadSDLEvents, MonadSDLTimer
                       )

instance MonadTrans SDLVideoT where
  lift = SDLVideoT

instance (MonadSDLTimer m, MonadSDLEvents m) => MonadSDLVideo (SDLVideoT m) where

withSDLVideo :: (MonadSDLEvents m, MonadSDLTimer m) => SDLVideoT m a -> m a
withSDLVideo = withSubSystem Video . runSDLVideoT
