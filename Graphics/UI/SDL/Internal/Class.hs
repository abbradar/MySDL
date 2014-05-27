{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.SDL.Internal.Class (
  MonadSDL(..),
  MonadSDLVideo(..),
  MonadSDLAudio(..),
  MonadSDLEvents(..)
  ) where

import Control.Monad.Base (MonadBase)

class MonadBase IO m => MonadSDL m where
class MonadSDL m => MonadSDLAudio m where
class MonadSDL m => MonadSDLEvents m where
class MonadSDLEvents m => MonadSDLVideo m where
