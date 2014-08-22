module Graphics.UI.SDL.Version
       ( Version(..)
       , compiledSDL
       , linkedSDL
       ) where

import Data.Word
import Control.Applicative ((<$>))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CUChar)
import Foreign.Storable (peekByteOff)

#include <SDL2/SDL_version.h>

data Version = Version !Word8 !Word8 !Word8
             deriving (Show, Eq, Ord)

{#pointer *SDL_version as CVersion #}

#c

void SDL_CompiledVersion(SDL_version* ver)
{
  SDL_VERSION(ver);
}

#endc

getVersion :: (CVersion -> IO ()) -> IO Version
getVersion get = allocaBytesAligned {#sizeof SDL_version #} {#alignof SDL_version #} $ \p -> do
  get p
  major <- fromIntegral <$> {#get SDL_version->major #} p
  minor <- fromIntegral <$> {#get SDL_version->minor #} p
  patch <- fromIntegral <$> {#get SDL_version->patch #} p
  return $ Version major minor patch

compiledSDL :: Version
compiledSDL = unsafeDupablePerformIO $ getVersion {#call unsafe SDL_CompiledVersion as ^ #}

linkedSDL :: Version
linkedSDL = unsafeDupablePerformIO $ getVersion {#call unsafe SDL_GetVersion as ^ #}
