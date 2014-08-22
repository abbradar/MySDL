{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Rendering.OpenGL.Safe.Shaders (
  prepareShader,
  safeLinkProgram
  ) where

import Control.Monad
import Control.Monad.Logger (MonadLogger, logDebugNS)
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Text (pack)
import Control.Monad.Base (MonadBase(..))
import Graphics.Rendering.OpenGL.GL

prepareShader :: (MonadLogger m, MonadBase IO m) => ShaderType -> ByteString -> m Shader
prepareShader t s = do
  p <- liftBase $ createShader t
  liftBase $ shaderSourceBS p $= s
  liftBase $ compileShader p
  pack <$> liftBase (get $ shaderInfoLog p) >>= logDebugNS "prepareShader"
  liftBase $ get (compileStatus p) >>= flip unless (fail "Shader compile error")
  return p

safeLinkProgram :: (MonadLogger m, MonadBase IO m) => Program -> m ()
safeLinkProgram pr = do
  liftBase $ linkProgram pr
  pack <$> liftBase (get $ programInfoLog pr) >>= logDebugNS "safeLinkProgram"
  liftBase $ get (linkStatus pr) >>= flip unless (fail "Program link error")
