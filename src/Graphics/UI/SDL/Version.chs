{-|
Description: Compiled and linked SDL versions.
-}

module Graphics.UI.SDL.Version
       ( Version(..)
       , compiledSDL
       , linkedSDL
       ) where

import Data.Word
import System.IO.Unsafe (unsafeDupablePerformIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CUChar(..))
import Foreign.Storable (peekByteOff)

#include <SDL2/SDL_version.h>
#include "sdl_misc.h"

-- | SDL version: major, minor and patch.
data Version = Version !Word8 !Word8 !Word8
             deriving (Show, Eq, Ord)

{#pointer *SDL_version as CVersion #}

getVersion :: CVersion -> IO Version
getVersion p = do
  (CUChar major) <- {#get SDL_version->major #} p
  (CUChar minor) <- {#get SDL_version->minor #} p
  (CUChar patch) <- {#get SDL_version->patch #} p
  return $ Version major minor patch

-- | Compiled SDL version.
compiledSDL :: Version
compiledSDL = unsafeDupablePerformIO $ do
  p <- {#call unsafe compiled_version as ^ #}
  getVersion p

-- | Linked SDL version.
linkedSDL :: Version
linkedSDL = unsafeDupablePerformIO $
            allocaBytesAligned {#sizeof SDL_version #} {#alignof SDL_version #} $ \p -> do
              {#call unsafe SDL_GetVersion as ^ #} p
              getVersion p
