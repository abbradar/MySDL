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
       ) where

import Control.Applicative ((<$>))
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
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

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Video.Monad
import Graphics.UI.SDL.Video.Window

{#import Graphics.UI.SDL.Video.Internal.Window #}

#include <SDL2/SDL_video.h>

{#enum SDL_GLattr as CGLAttr {underscoreToCase} #}

class GLAttribute a b | a -> b where
  toCAttr :: a -> CGLAttr
  toCValue :: a -> b -> Int
  fromCValue :: a -> Int -> b
  
getGLAttribute :: (GLAttribute a b, MonadSDLVideo m) => a -> m b
getGLAttribute a = liftBase $ (fromCValue a) <$> fromIntegral <$> snd <$> sdlCall "SDL_GL_GetAttribute"
                   (sDLGLGetAttribute $ toCAttr a) ((== 0) . fst)
  where {#fun unsafe SDL_GL_GetAttribute as ^ { `CGLAttr', alloca- `CInt' peek*} -> `Int' #}

setGLAttribute :: (GLAttribute a b, MonadSDLVideo m) => a -> b -> m ()
setGLAttribute a v = liftBase $ sdlCode "SDL_GL_SetAttribute" $
                     sDLGLSetAttribute (toCAttr a) (toCValue a v)
  where {#fun unsafe SDL_GL_SetAttribute as ^ { `CGLAttr', `Int' } -> `Int' #}

data GLAttrInt = ContextMinorVersion
               | ContextMajorVersion
               deriving (Show, Eq)

instance GLAttribute GLAttrInt Int where
  toCValue _ = id
  fromCValue _ = id

  toCAttr ContextMinorVersion = SdlGlContextMinorVersion
  toCAttr ContextMajorVersion = SdlGlContextMajorVersion

data GLAttrProfile = ContextProfile
                   deriving (Show, Eq)

{#enum SDL_GLprofile as GLProfile {underscoreToCase} deriving (Show, Eq) #}

instance GLAttribute GLAttrProfile GLProfile where
  toCValue _ = fromEnum
  fromCValue _ = toEnum

  toCAttr _ = SdlGlContextProfileMask

{#pointer SDL_GLContext as CGLContext foreign #}

data GLContext = GLContext !Window !CGLContext

foreign import ccall unsafe "SDL2/SDL_video.h &SDL_GL_DeleteContext"
  pDeleteContext :: FunPtr (Ptr () -> IO ())

createGLContext :: MonadSDLVideo m => Window -> m GLContext
createGLContext w = liftBase $ mask_ $ do
  p <- sdlObject "SDL_GL_CreateContext" id $ sDLGLCreateContext w
  addForeignPtrFinalizer pDeleteContext p
  return $ GLContext w p

  where {#fun unsafe SDL_GL_CreateContext as ^
         { `CWindow' } -> `CGLContext' #}
  
freeGLContext :: MonadSDLVideo m => GLContext -> m ()
freeGLContext (GLContext (CWindow wp) p) = liftBase $ do
  finalizeForeignPtr p
  -- This guarantees that window will be finalized no earlier than context.
  -- TODO: CHECK THIS! Notes in ForeignPtr documentation point that this may not work.
  touchForeignPtr wp

glContextWindow :: GLContext -> Window
glContextWindow (GLContext w _) = w

glSetCurrent :: MonadSDLVideo m => GLContext -> m ()
glSetCurrent (GLContext w p) =
  liftBase $ sdlCode "SDL_GL_MakeCurrent" $ sDLGLMakeCurrent w p

  where {#fun unsafe SDL_GL_MakeCurrent as ^
         { `CWindow', `CGLContext' } -> `Int' #}

glSwap :: MonadSDLVideo m => Window -> m ()
glSwap = liftBase . sDLGLSwapWindow
  where {#fun SDL_GL_SwapWindow as ^ { `CWindow' } -> `()' #}
