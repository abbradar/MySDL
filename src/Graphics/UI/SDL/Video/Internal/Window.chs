{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.UI.SDL.Video.Internal.Window where

import Foreign.ForeignPtr.Safe (ForeignPtr,
                                withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CUInt)

#include <SDL2/SDL_video.h>

{#pointer *SDL_Window as CWindow foreign newtype #}

newtype Window = Window CWindow
newtype GLWindow = GLWindow CWindow
type SomeWindow = Either Window GLWindow

class SDLWindow a where
  toCWindow :: a -> CWindow
  toSomeWindow :: a -> SomeWindow

instance SDLWindow Window where
  toCWindow (Window w) = w
  toSomeWindow = Left

instance SDLWindow GLWindow where
  toCWindow (GLWindow w) = w
  toSomeWindow = Right

instance SDLWindow SomeWindow where
  toCWindow (Left a) = toCWindow a
  toCWindow (Right a) = toCWindow a
  toSomeWindow = id

newtype WindowID = WindowID CUInt
                 deriving (Eq, Show, Ord)
