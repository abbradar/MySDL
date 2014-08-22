{-# LANGUAGE FlexibleContexts #-}

module Graphics.Rendering.OpenGL.Safe.Buffer (
  prepareShader,
  safeLinkProgram
  ) where

import Graphics.Rendering.OpenGL.GL

import Graphics.Rendering.OpenGL.Safe

data SafeBuffer = SafeBuffer Buffer (IORef ForeignPtr)
                  deriving (Eq, Show)

createEmptyBuffer :: BufferTarget -> 

prepareShader :: MonadBase IO m => ShaderType -> ByteString -> OpenGLT m Shader
prepareShader t s = do
  p <- liftBase $ createShader t
  liftBase $ shaderSourceBS p $= s
  liftBase $ do
    compileShader p
    get (shaderInfoLog p) >>= putStr
    get (compileStatus p) >>= flip unless (fail "Shader compile error")
    return p

safeLinkProgram :: MonadBase IO m => Program -> OpenGLT m ()
safeLinkProgram pr = liftBase $ do
  linkProgram pr
  get (programInfoLog pr) >>= putStr
  get (linkStatus pr) >>= flip unless (fail "Program link error")
