{-|
Description: OpenGL-related functions.
-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Graphics.UI.SDL.Video.OpenGL
       ( GLAttribute
       , getGLAttribute
       , setGLAttribute
       , GLAttrInt(..)
       , GLAttrContextFlags(..)
       , GLContextFlag(..)
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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (mask_)
import Data.Int
import Data.Bits

import Data.Enum.Num
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Video.Monad
import Graphics.UI.SDL.Video.Window

{#import Graphics.UI.SDL.Video.Internal.Window #}

#include <SDL2/SDL_video.h>

{#enum SDL_GLattr as CGLAttr {underscoreToCase} #}

-- | OpenGL attribute class. Each attribute type @a@ has its data type @b@.
class GLAttribute a b | a -> b where
  toCAttr :: a -> CGLAttr
  toCValue :: a -> b -> CInt
  fromCValue :: a -> CInt -> b

-- | Get current value of an OpenGL attribute.
getGLAttribute :: (GLAttribute a b, MonadSDLVideo m) => a -> m b
getGLAttribute a = liftIO $ (fromCValue a) <$> snd <$> sdlCall "SDL_GL_GetAttribute"
                   (sDLGLGetAttribute $ toCAttr a) ((== 0) . fst)
  where {#fun unsafe SDL_GL_GetAttribute as ^ { `CGLAttr', alloca- `CInt' peek*} -> `Int' #}

-- | Sets new value of an OpenGL attribute. This should be done before creating a window.
setGLAttribute :: (GLAttribute a b, MonadSDLVideo m) => a -> b -> m ()
setGLAttribute a v = liftIO $ sdlCode "SDL_GL_SetAttribute" $
                     sDLGLSetAttribute (toCAttr a) (toCValue a v)
-- TODO: Remove "id" after issue #83 is resolved in c2hs
  where {#fun unsafe SDL_GL_SetAttribute as ^ { `CGLAttr', id `CInt' } -> `Int' #}

-- | Integer OpenGL attributes.
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

-- | OpenGL context flags attribute.
data GLAttrContextFlags = ContextFlags
                        deriving (Show, Eq)

-- | Context flags.
{#enum SDL_GLcontextFlag as GLContextFlag {underscoreToCase} deriving (Show, Eq, Bounded) #}

instance GLAttribute GLAttrContextFlags [GLContextFlag] where
  toCValue _ = foldr1 (.|.) . map fromEnum'
  fromCValue _ val = filter (\m -> val .&. fromEnum' m /= 0) [minBound..maxBound]

  toCAttr ContextFlags = SdlGlContextFlags

-- | OpenGL profile attribute.
data GLAttrProfile = ContextProfile
                   deriving (Show, Eq)

-- | OpenGL profiles.
{#enum SDL_GLprofile as GLProfile {underscoreToCase} deriving (Show, Eq) #}

instance GLAttribute GLAttrProfile GLProfile where
  toCValue _ = fromEnum'
  fromCValue _ = toEnum'

  toCAttr ContextProfile = SdlGlContextProfileMask

-- | OpenGL flag-like attributes.
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

-- | OpenGL context for a window.
data GLContext = GLContext !GLWindow !CGLContext

foreign import ccall unsafe "SDL2/SDL_video.h &SDL_GL_DeleteContext"
  pDeleteContext :: FunPtr (Ptr () -> IO ())

-- | Create OpenGL context for a given accelerated 'GLWindow'.
createGLContext :: MonadSDLVideo m => GLWindow -> m GLContext
createGLContext ww@(GLWindow w) = liftIO $ mask_ $ do
  p <- sdlObject "SDL_GL_CreateContext" id $ sDLGLCreateContext w
  addForeignPtrFinalizer pDeleteContext p
  return $ GLContext ww p

  where {#fun unsafe SDL_GL_CreateContext as ^
         { `CWindow' } -> `CGLContext' #}

-- | Destroy OpenGL context.
freeGLContext :: MonadSDLVideo m => GLContext -> m ()
freeGLContext (GLContext (GLWindow (CWindow wp)) p) = liftIO $ do
  finalizeForeignPtr p
  -- This guarantees that window will be finalized no earlier than context.
  -- TODO: CHECK THIS! Notes in ForeignPtr documentation point that this may not work.
  touchForeignPtr wp

-- | Get window for a given context.
glContextWindow :: GLContext -> GLWindow
glContextWindow (GLContext w _) = w

-- | Set a context as current (meaning, OpenGL functions will work with it).
glSetCurrent :: MonadSDLVideo m => GLContext -> m ()
glSetCurrent (GLContext (GLWindow w) p) =
  liftIO $ sdlCode "SDL_GL_MakeCurrent" $ sDLGLMakeCurrent w p

  where {#fun unsafe SDL_GL_MakeCurrent as ^
         { `CWindow', `CGLContext' } -> `Int' #}

-- | Swap OpenGL buffers in a 'GLWindow'.
glSwap :: MonadSDLVideo m => GLWindow -> m ()
glSwap (GLWindow w) = liftIO $ sDLGLSwapWindow w
  where {#fun SDL_GL_SwapWindow as ^ { `CWindow' } -> `()' #}

-- | Get real drawable area size for a window.
glGetDrawableSize :: MonadSDLVideo m => GLWindow -> m Size
glGetDrawableSize (GLWindow w) = liftIO $ do
  (CInt x, CInt y) <- sDLGLGetDrawableSize w
  return $ P x y
  
  where {#fun unsafe SDL_GL_GetDrawableSize as ^
         { `CWindow'
         , alloca- `CInt' peek*
         , alloca- `CInt' peek*
         } -> `()' #}
