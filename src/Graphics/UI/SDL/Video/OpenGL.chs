{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Graphics.UI.SDL.Video.OpenGL
       ( GLAttribute
       , getGLAttribute
       , setGLAttribute
       , GLAttrInt(..)
       , GLAttrProfile(..)
       , GLProfile(..)
       , GLContext
       , createGLContext
       , freeGLContext
       , glContextWindow
       , glSetCurrent
       , glSwap
       , glGetDrawableSize
       ) where

import Control.Applicative ((<$>))
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Storable (peek)
import Foreign.ForeignPtr.Safe (ForeignPtr,
                                newForeignPtr_,
                                addForeignPtrFinalizer,
                                withForeignPtr,
                                finalizeForeignPtr,
                                touchForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import Control.Monad.Base (liftBase)
import Control.Exception.Lifted (mask_)
import Graphics.Rendering.OpenGL.GL.Tensor (Vector2(..))
import Data.Int

import Data.Enum.Num
import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Video.Monad
import Graphics.UI.SDL.Video.Window

{#import Graphics.UI.SDL.Video.Internal.Window #}

#include <SDL2/SDL_video.h>

{#enum SDL_GLattr as CGLAttr {underscoreToCase} #}

class GLAttribute a b | a -> b where
  toCAttr :: a -> CGLAttr
  toCValue :: a -> b -> CInt
  fromCValue :: a -> CInt -> b
  
getGLAttribute :: (GLAttribute a b, MonadSDLVideo m) => a -> m b
getGLAttribute a = liftBase $ (fromCValue a) <$> snd <$> sdlCall "SDL_GL_GetAttribute"
                   (sDLGLGetAttribute $ toCAttr a) ((== 0) . fst)
  where {#fun unsafe SDL_GL_GetAttribute as ^ { `CGLAttr', alloca- `CInt' peek*} -> `Int' #}

setGLAttribute :: (GLAttribute a b, MonadSDLVideo m) => a -> b -> m ()
setGLAttribute a v = liftBase $ sdlCode "SDL_GL_SetAttribute" $
                     sDLGLSetAttribute (toCAttr a) (toCValue a v)
-- TODO: Remove "id" after issue #83 is resolved in c2hs
  where {#fun unsafe SDL_GL_SetAttribute as ^ { `CGLAttr', id `CInt' } -> `Int' #}

data GLAttrInt = RedSize
               | GreenSize
               | BlueSize
               | AlphaSize
               | BufferSize
               | DepthSize
               | StencilSize
               | AccumRedSize
               | AccumGreenSize
               | AccumBlueSize
               | AccumAlphaSize
               | MultiSampleBuffers
               | MultiSampleSamples
               | ContextMinorVersion
               | ContextMajorVersion
               deriving (Show, Eq)

instance GLAttribute GLAttrInt Int32 where
  toCValue _ = CInt
  fromCValue _ (CInt a) = a

  toCAttr RedSize = SdlGlRedSize
  toCAttr GreenSize = SdlGlGreenSize
  toCAttr BlueSize = SdlGlBlueSize
  toCAttr AlphaSize = SdlGlAlphaSize
  toCAttr BufferSize = SdlGlBufferSize
  toCAttr DepthSize = SdlGlDepthSize
  toCAttr StencilSize = SdlGlStencilSize
  toCAttr AccumRedSize = SdlGlAccumRedSize
  toCAttr AccumGreenSize = SdlGlAccumGreenSize
  toCAttr AccumBlueSize = SdlGlAccumBlueSize
  toCAttr AccumAlphaSize = SdlGlAccumAlphaSize
  toCAttr MultiSampleBuffers = SdlGlMultisamplebuffers
  toCAttr MultiSampleSamples = SdlGlMultisamplesamples
  toCAttr ContextMinorVersion = SdlGlContextMinorVersion
  toCAttr ContextMajorVersion = SdlGlContextMajorVersion

data GLAttrProfile = ContextProfile
                   deriving (Show, Eq)

{#enum SDL_GLprofile as GLProfile {underscoreToCase} deriving (Show, Eq) #}

instance GLAttribute GLAttrProfile GLProfile where
  toCValue _ = fromEnum'
  fromCValue _ = toEnum'

  toCAttr ContextProfile = SdlGlContextProfileMask

data GLAttrBool = DoubleBuffer
                | Stereo
                | AcceleratedVisual
                | ShareWithCurrentContext
                | FramebufferSRGBCapable
                deriving (Show, Eq)

instance GLAttribute GLAttrBool Bool where
  toCValue _ = fromBool
  fromCValue _ = toBool

  toCAttr DoubleBuffer = SdlGlDoublebuffer
  toCAttr Stereo = SdlGlStereo
  toCAttr AcceleratedVisual = SdlGlAcceleratedVisual
  toCAttr ShareWithCurrentContext = SdlGlShareWithCurrentContext
  toCAttr FramebufferSRGBCapable = SdlGlFramebufferSrgbCapable

{#pointer SDL_GLContext as CGLContext foreign #}

data GLContext = GLContext !GLWindow !CGLContext

foreign import ccall unsafe "SDL2/SDL_video.h &SDL_GL_DeleteContext"
  pDeleteContext :: FunPtr (Ptr () -> IO ())

createGLContext :: MonadSDLVideo m => GLWindow -> m GLContext
createGLContext ww@(GLWindow w) = liftBase $ mask_ $ do
  p <- sdlObject "SDL_GL_CreateContext" id $ sDLGLCreateContext w
  addForeignPtrFinalizer pDeleteContext p
  return $ GLContext ww p

  where {#fun unsafe SDL_GL_CreateContext as ^
         { `CWindow' } -> `CGLContext' #}
  
freeGLContext :: MonadSDLVideo m => GLContext -> m ()
freeGLContext (GLContext (GLWindow (CWindow wp)) p) = liftBase $ do
  finalizeForeignPtr p
  -- This guarantees that window will be finalized no earlier than context.
  -- TODO: CHECK THIS! Notes in ForeignPtr documentation point that this may not work.
  touchForeignPtr wp

glContextWindow :: GLContext -> GLWindow
glContextWindow (GLContext w _) = w

glSetCurrent :: MonadSDLVideo m => GLContext -> m ()
glSetCurrent (GLContext (GLWindow w) p) =
  liftBase $ sdlCode "SDL_GL_MakeCurrent" $ sDLGLMakeCurrent w p

  where {#fun unsafe SDL_GL_MakeCurrent as ^
         { `CWindow', `CGLContext' } -> `Int' #}

glSwap :: MonadSDLVideo m => GLWindow -> m ()
glSwap (GLWindow w) = liftBase $ sDLGLSwapWindow w
  where {#fun SDL_GL_SwapWindow as ^ { `CWindow' } -> `()' #}

glGetDrawableSize :: MonadSDLVideo m => GLWindow -> m Size
glGetDrawableSize (GLWindow w) = liftBase $ do
  (CInt x, CInt y) <- sDLGLGetDrawableSize w
  return $ Vector2 x y
  
  where {#fun unsafe SDL_GL_GetDrawableSize as ^
         { `CWindow'
         , alloca- `CInt' peek*
         , alloca- `CInt' peek*
         } -> `()' #}
