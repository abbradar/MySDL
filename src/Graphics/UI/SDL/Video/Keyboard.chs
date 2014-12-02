{-|
Description: Keyboard, keys, scan- and keycodes.
-}

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
import Data.ByteString (ByteString, packCString, useAsCString)
import qualified Data.ByteString as B
import Data.ByteString.Unsafe (unsafePackCString)
import Foreign.C.Types (CInt(..), CChar(..))
import Foreign.Ptr (Ptr)
import Control.Monad.IO.Class

import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Video.Internal.Surface
import Graphics.UI.SDL.Video.Monad

{#import Graphics.UI.SDL.Video.Keyboard.Types #}

#include <SDL2/SDL_keyboard.h>

-- | Key name from key code.
keyName :: MonadIO m => KeyCode -> m ByteString
keyName c = liftIO $ sdlCall "SDL_GetKeyName"
            (sDLGetKeyName $ fromEnum c) (not . B.null)
  where {#fun unsafe SDL_GetKeyName as ^
         {`Int'} -> `ByteString' packCString* #}

-- | Key code from key name.
keyFromName :: MonadIO m => ByteString -> m KeyCode
keyFromName c = liftIO $ sdlCall "SDL_GetKeyFromName"
                (toEnum <$> sDLGetKeyFromName c) (/= SdlkUnknown)
  where {#fun unsafe SDL_GetKeyFromName as ^
         {useAsCString* `ByteString'} -> `Int' #}

-- | Key name from scancode.
scancodeName :: MonadIO m => ScanCode -> m ByteString
scancodeName c = liftIO $ sdlCall "SDL_GetScancodeName"
                 (sDLGetScancodeName $ fromEnum c) (not . B.null)
  where {#fun unsafe SDL_GetScancodeName as ^
         {`Int'} -> `ByteString' unsafePackCString* #}

-- | Scancode to key name.
scancodeFromName :: MonadIO m => ByteString -> m ScanCode
scancodeFromName c = liftIO $ sdlCall "SDL_GetScancodeFromName"
                     (toEnum <$> sDLGetScancodeFromName c) (/= SdlScancodeUnknown)
  where {#fun unsafe SDL_GetScancodeFromName as ^
         {useAsCString* `ByteString'} -> `Int' #}

-- | Key scancode from key code (subject to change with translation table).
scancodeFromKey :: MonadSDLVideo m => KeyCode -> m ScanCode
scancodeFromKey = liftIO . liftM toEnum . sDLGetScancodeFromKey . fromEnum
  where {#fun unsafe SDL_GetScancodeFromKey as ^
         {`Int'} -> `Int' #}

-- | Key code from scancode (subject to change with translation table).
keyFromScancode :: MonadSDLVideo m => ScanCode -> m KeyCode
keyFromScancode = liftIO . liftM toEnum . sDLGetKeyFromScancode . fromEnum
  where {#fun unsafe SDL_GetKeyFromScancode as ^
         {`Int'} -> `Int' #}

-- | Enable receiving of text input events from SDL (along with usual keyboard events).
startTextInput :: MonadSDLVideo m => m ()
startTextInput = liftIO {#call unsafe SDL_StartTextInput as ^ #}

-- | Disable receiving of text input events from SDL (along with usual keyboard events).
stopTextInput :: MonadSDLVideo m => m ()
stopTextInput = liftIO {#call unsafe SDL_StopTextInput as ^ #}

-- | Hint to IME and/or on-screen keyboard where characters would be placed.
setTextInputRect :: MonadSDLVideo m => Rect -> m ()
setTextInputRect r = liftIO $ withCRect r {#call unsafe SDL_SetTextInputRect as ^ #}
