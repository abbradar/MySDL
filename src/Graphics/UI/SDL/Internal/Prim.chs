module Graphics.UI.SDL.Internal.Prim
       ( SDLBool
       , fromSDLBool
       , toSDLBool
       , SDLError(..)
       , sdlCall
       , sdlCode
       , sdlBool
       , sdlObject
       , freeSDL
       ) where

import Data.Typeable
import Control.Monad
import Control.Monad.Catch
import Foreign.ForeignPtr.Safe (ForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.C.Types (CChar)
import Foreign.C.String (peekCString)

#include <SDL2/SDL.h>

{#enum SDL_bool as SDLBool {underscoreToCase} deriving (Eq, Show) #}

fromSDLBool :: SDLBool -> Bool
fromSDLBool SdlTrue = True
fromSDLBool SdlFalse = False

toSDLBool :: Bool -> SDLBool
toSDLBool True = SdlTrue
toSDLBool False = SdlFalse

data SDLError = SDLError String String
              deriving (Show, Typeable)

instance Exception SDLError

sdlCall :: String -> IO a -> (a -> Bool) -> IO a
sdlCall str call test = do
  r <- call
  unless (test r) $ do
    err <- {#call unsafe SDL_GetError as ^#} >>= peekCString
    throwM $ SDLError str err
  return r

sdlCode :: String -> IO Int -> IO ()
sdlCode str call = sdlCall str call (== 0) >> return ()

sdlBool :: String -> IO SDLBool -> IO ()
sdlBool str call = sdlCall str call (== SdlTrue) >> return ()

sdlObject :: String -> (a -> ForeignPtr b) -> IO a -> IO a
sdlObject str f call = do
  r <- sdlCall str call $ (/= nullPtr) . unsafeForeignPtrToPtr . f
  touchForeignPtr $ f r
  return r

freeSDL :: Ptr a -> IO ()
freeSDL = {#call SDL_free as ^ #} . castPtr
