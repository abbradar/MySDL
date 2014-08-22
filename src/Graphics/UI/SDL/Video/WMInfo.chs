{-# LANGUAGE LambdaCase #-}

module Graphics.UI.SDL.Video.WMInfo
       ( WMInfo
       , windowWMInfo
       ) where

import Control.Applicative ((<$>))
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Storable (peekByteOff)
import Foreign.C.Types (CInt(..))

import Data.Enum.Num
import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Video.Window

{#import Graphics.UI.SDL.Video.Internal.SysWM #}
{#import Graphics.UI.SDL.Video.Internal.Window #}

#include <SDL2/SDL_version.h>
#include <SDL2/SDL_syswm.h>

#c

typedef struct SDL_SysWMinfo SDL_SysWMinfo_t;

#endc

-- TODO: Get more info
data WMInfo = Windows
            | WinRT
            | X11
            | DirectFB
            | Cocoa
            | UIKit
            | Wayland
            | Mir
              
--{#pointer *SDL_SysWMinfo_t as CWMInfo #}

windowWMInfo :: Window -> IO (Maybe WMInfo)
windowWMInfo (CWindow wp) =
  allocaBytesAligned {#sizeof SDL_SysWMinfo_t #} {#alignof SDL_SysWMinfo_t #} $
  \p -> withForeignPtr wp $ \w -> do
    let v = p `plusPtr` {#offsetof SDL_SysWMinfo->version #}
    {#call unsafe SDL_GetVersion as ^ #} v
    sdlBool "SDL_GetWindowWMInfo" $ toEnum' <$> {#call unsafe SDL_GetWindowWMInfo as ^ #} w p
    toEnum' <$> {#get SDL_SysWMinfo->subsystem #} p >>= \case
      SdlSyswmUnknown -> return Nothing
      SdlSyswmWindows -> return $ Just Windows
      SdlSyswmWinrt -> return $ Just WinRT
      SdlSyswmX11 -> return $ Just X11
      SdlSyswmDirectfb -> return $ Just DirectFB
      SdlSyswmCocoa -> return $ Just Cocoa
      SdlSyswmUikit -> return $ Just UIKit
      SdlSyswmWayland -> return $ Just Wayland
      SdlSyswmMir -> return $ Just Mir
      e -> fail $ "Unknown platform type: " ++ show e
