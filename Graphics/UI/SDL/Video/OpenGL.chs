module Graphics.UI.SDL.Video.OpenGL (
  GLAttrInt(..),
  glGetAttributeInt,
  glSetAttributeInt
  ) where

import Control.Applicative ((<$>))
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Control.Monad.Base (liftBase)
import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Video

#include <SDL2/SDL_video.h>

{#enum SDL_GLattr as CGLAttr {underscoreToCase} #}

data GLAttrInt = ContextMinorVersion
               | ContextMajorVersion
               deriving (Show, Eq)

toCAttrInt :: GLAttrInt -> CGLAttr
toCAttrInt ContextMinorVersion = SdlGlContextMinorVersion
toCAttrInt ContextMajorVersion = SdlGlContextMajorVersion

data GLAttr = GLAttrInt GLAttrInt Int
            deriving (Show, Eq)

glGetAttribute :: MonadSDLVideo m => CGLAttr -> m Int
glGetAttribute a = liftBase $ fromIntegral <$> snd <$> sdlCall "SDL_GL_GetAttribute"
                   (sDLGLGetAttribute a) ((== 0) . fst)
  where {#fun unsafe SDL_GL_GetAttribute as ^ { `CGLAttr', alloca- `CInt' peek*} -> `Int' #}

glSetAttribute :: MonadSDLVideo m => CGLAttr -> Int -> m ()
glSetAttribute a v = liftBase $ sdlCode "SDL_GL_SetAttribute" $ sDLGLSetAttribute a v
  where {#fun unsafe SDL_GL_SetAttribute as ^ { `CGLAttr', `Int' } -> `Int' #}

glGetAttributeInt :: MonadSDLVideo m => GLAttrInt -> m Int
glGetAttributeInt = glGetAttribute . toCAttrInt

glSetAttributeInt :: MonadSDLVideo m => GLAttrInt -> Int -> m ()
glSetAttributeInt a = glSetAttribute (toCAttrInt a)
