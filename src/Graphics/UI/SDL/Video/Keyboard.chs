module Graphics.UI.SDL.Video.Keyboard
         ( keyName
         , keyFromName
         , scancodeName
         , scancodeFromName
         , scancodeFromKey
         , keyFromScancode

         , startTextInput
         , stopTextInput
         , setTextInputRect
         ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Concurrent.MVar ( MVar
                               , newMVar
                               , withMVar
                               )
import Data.ByteString (ByteString, packCString, useAsCString)
import qualified Data.ByteString as B
import Data.ByteString.Unsafe (unsafePackCString)
import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import Foreign.C.Types (CInt(..), CChar(..))
import Foreign.Ptr (Ptr)
import Control.Monad.IO.Class

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Video.Internal.Surface
import Graphics.UI.SDL.Class

{#import Graphics.UI.SDL.Video.Keyboard.Types #}

#include <SDL2/SDL_keyboard.h>

-- This function needs to be guarded to be thread-safe. Global variables!
keyNameGuard :: MVar ()
{-# NOINLINE keyNameGuard #-}
keyNameGuard = unsafePerformIO $ newMVar ()

keyName :: KeyCode -> ByteString
keyName c = unsafeDupablePerformIO $ withMVar keyNameGuard $ const $
            sdlCall "SDL_GetKeyName" (sDLGetKeyName $ fromEnum c) (not . B.null)
  where {#fun unsafe SDL_GetKeyName as ^
         {`Int'} -> `ByteString' packCString* #}

keyFromName :: ByteString -> KeyCode
keyFromName c = unsafeDupablePerformIO $ sdlCall "SDL_GetKeyFromName"
                (toEnum <$> sDLGetKeyFromName c) (/= SdlkUnknown)
  where {#fun unsafe SDL_GetKeyFromName as ^
         {useAsCString* `ByteString'} -> `Int' #}

-- Unlike 'keyName', this is thread-safe
scancodeName :: ScanCode -> ByteString
scancodeName c = unsafeDupablePerformIO $
                 sdlCall "SDL_GetScancodeName" (sDLGetScancodeName $ fromEnum c) (not . B.null)
  where {#fun unsafe SDL_GetScancodeName as ^
         {`Int'} -> `ByteString' unsafePackCString* #}

scancodeFromName :: ByteString -> ScanCode
scancodeFromName c = unsafeDupablePerformIO $ sdlCall "SDL_GetScancodeFromName"
                     (toEnum <$> sDLGetScancodeFromName c) (/= SdlScancodeUnknown)
  where {#fun unsafe SDL_GetScancodeFromName as ^
         {useAsCString* `ByteString'} -> `Int' #}

scancodeFromKey :: MonadSDLVideo m => KeyCode -> m ScanCode
scancodeFromKey = liftIO . liftM toEnum . sDLGetScancodeFromKey . fromEnum
  where {#fun unsafe SDL_GetScancodeFromKey as ^
         {`Int'} -> `Int' #}

keyFromScancode :: MonadSDLVideo m => ScanCode -> m KeyCode
keyFromScancode = liftIO . liftM toEnum . sDLGetKeyFromScancode . fromEnum
  where {#fun unsafe SDL_GetKeyFromScancode as ^
         {`Int'} -> `Int' #}

startTextInput :: MonadSDLVideo m => m ()
startTextInput = liftIO {#call unsafe SDL_StartTextInput as ^ #}

stopTextInput :: MonadSDLVideo m => m ()
stopTextInput = liftIO {#call unsafe SDL_StopTextInput as ^ #}

setTextInputRect :: MonadSDLVideo m => Rect -> m ()
setTextInputRect r = liftIO $ withCRect r {#call unsafe SDL_SetTextInputRect as ^ #}
