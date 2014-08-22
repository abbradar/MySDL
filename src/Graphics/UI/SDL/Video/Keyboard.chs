module Graphics.UI.SDL.Video.Keyboard
         ( KeyCode(..)
         , ScanCode(..)
         , KeyMod(..)
         , KeySym(..)
         , keyName
         , keyFromName
         , scancodeName
         , scancodeFromName
         , scancodeFromKey
         , keyFromScancode
         ) where

import Control.Concurrent.MVar (MVar,
                                newMVar,
                                takeMVar,
                                putMVar)
import Control.Exception (bracket_)
import Control.Monad.Base (liftBase)
import Data.ByteString (ByteString, packCString)
import qualified Data.ByteString as B
import Data.ByteString.Unsafe (unsafePackCString)
import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import Foreign.C.Types (CInt(..), CChar(..))
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import Data.BitSet.Word (BitSet)

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Class

#include <SDL2/SDL_keyboard.h>
#include <SDL2/SDL_keycode.h>

{#enum SDLK_UNKNOWN as KeyCode {underscoreToCase} deriving (Eq, Show) #}

{#enum SDL_Scancode as ScanCode {underscoreToCase} deriving (Eq, Show) #}

data KeyMod = ModLShift
            | ModRShift
            | ModLCtrl
            | ModRCtrl
            | ModLAlt
            | ModRAlt
            | ModLGUI
            | ModRGUI
            | ModNum
            | ModCaps
            | ModMode
            deriving (Eq, Show, Enum)

data KeySym = KeySym { scanCode :: !ScanCode
                     , keyCode :: !KeyCode
                     , keyMod :: !(BitSet KeyMod)
                     }
            deriving (Eq, Show)

-- This function needs to be guarded to be thread-safe. Global variables!
keyNameGuard :: MVar ()
{-# NOINLINE keyNameGuard #-}
keyNameGuard = unsafePerformIO $ newMVar ()

keyName :: KeyCode -> ByteString
keyName c = unsafeDupablePerformIO $ bracket_
            (takeMVar keyNameGuard)
            (putMVar keyNameGuard ())
            $ sdlCall "SDL_GetKeyName" (sDLGetKeyName c) (not . B.null)
  where {#fun unsafe SDL_GetKeyName as ^
         {`KeyCode'} -> `ByteString' packCString* #}

keyFromName :: ByteString -> KeyCode
keyFromName c = unsafeDupablePerformIO $ B.useAsCString c $
                \s -> sdlCall "SDL_GetKeyFromName" (sDLGetKeyFromName s)
                      (/= SdlkUnknown)
  where {#fun unsafe SDL_GetKeyFromName as ^
         {id `CString'} -> `KeyCode' #}

-- Unlike 'keyName', this is thread-safe
scancodeName :: ScanCode -> ByteString
scancodeName c = unsafeDupablePerformIO $
                 sdlCall "SDL_GetScancodeName" (sDLGetScancodeName c) (not . B.null)
  where {#fun unsafe SDL_GetScancodeName as ^
         {`ScanCode'} -> `ByteString' unsafePackCString* #}

scancodeFromName :: ByteString -> ScanCode
scancodeFromName c = unsafeDupablePerformIO $ B.useAsCString c $
                     \s -> sdlCall "SDL_GetScancodeFromName" (sDLGetScancodeFromName s)
                           (/= SdlScancodeUnknown)
  where {#fun unsafe SDL_GetScancodeFromName as ^
         {id `CString'} -> `ScanCode' #}

scancodeFromKey :: MonadSDLVideo m => KeyCode -> m ScanCode
scancodeFromKey = liftBase . sDLGetScancodeFromKey
  where {#fun unsafe SDL_GetScancodeFromKey as ^
         {`KeyCode'} -> `ScanCode' #}

keyFromScancode :: MonadSDLVideo m => ScanCode -> m KeyCode
keyFromScancode = liftBase . sDLGetKeyFromScancode
  where {#fun unsafe SDL_GetKeyFromScancode as ^
         {`ScanCode'} -> `KeyCode' #}
