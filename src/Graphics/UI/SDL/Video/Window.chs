{-|
Description: SDL window handling.
-}

module Graphics.UI.SDL.Video.Window
       ( Window
       , GLWindow
       , SomeWindow
       , SDLWindow(toSomeWindow)
       , PositionHint(..)
       , WindowFlags(..)
       , createWindow
       , freeWindow
       , windowFlags
       , WindowID
       , getWindowID
       , getWindowFromID
       , getWindowPosition
       , getWindowSize
       , setWindowSize
       ) where

import Control.Monad
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Foreign.ForeignPtr.Safe (newForeignPtr_,
                                addForeignPtrFinalizer,
                                finalizeForeignPtr,
                                FinalizerPtr)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CChar(..), CInt(..), CUInt(..))
import Foreign.C.String (CString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Control.Monad.Catch
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative ((<$>))
import Data.Word
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import System.Mem.Weak (Weak, mkWeak, deRefWeak)
import Control.Monad.IO.Class

import Data.Enum.Num
import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Video.Monad
import Graphics.UI.SDL.Types

{#import Graphics.UI.SDL.Video.Internal.Window #}

#include <SDL2/SDL_video.h>

-- c2hs does not support FunPtr's yet...
foreign import ccall unsafe "SDL2/SDL_video.h &SDL_DestroyWindow"
  pDestroyWindow :: FinalizerPtr CWindow

{#enum define SDLWindowPos { SDL_WINDOWPOS_CENTERED as Centered'
                           , SDL_WINDOWPOS_UNDEFINED as Undefined'
                           } #}

-- | Window position hint.
data PositionHint = Abs !Int32
                  | Centered
                  | NoHint
                deriving (Show, Eq)

fromPositionHint :: PositionHint -> CInt
fromPositionHint (Abs x) = CInt x
fromPositionHint Centered = fromEnum' Centered'
fromPositionHint NoHint = fromEnum' Undefined'

{#enum SDL_WindowFlags as WindowFlags {underscoreToCase} deriving (Eq, Show, Bounded) #}

-- There is no "refcount" field in SDL_Window and we can deceive GC by using getWindowFromID.
-- Here goes another global state.
windows :: MVar (Map WindowID (Weak SomeWindow))
{-# NOINLINE windows #-}
windows = unsafePerformIO $ newMVar Map.empty

-- | Create a window.
createWindow :: MonadSDLVideo m => ByteString -> Point PositionHint -> Size -> [WindowFlags] -> m SomeWindow
createWindow name (P x y) (P w h) fs = do
  cw <- liftIO $ mask_ $ do
    let f = foldr ((.|.) . fromEnum') 0 fs
    cw@(CWindow wh) <- unsafeUseAsCString name $ \cn ->
      sdlObject "SDL_CreateWindow" (\case CWindow a -> a) $ sDLCreateWindow cn x y w h f
    addForeignPtrFinalizer pDestroyWindow wh
    return cw
  let win = (if SdlWindowOpengl `elem` fs then toSomeWindow . GLWindow else toSomeWindow . Window) cw
  wid <- getWindowID win
  liftIO $ do
    p <- mkWeak cw win $ Just $ modifyMVar_ windows $ return . Map.delete wid
    modifyMVar_ windows $ return . Map.insert wid p
    return win

  where {#fun unsafe SDL_CreateWindow as ^
         { id `CString'
         , fromPositionHint `PositionHint', fromPositionHint `PositionHint'
         , `Int32', `Int32'
         , `Word32' } -> `CWindow' #}

-- | Destroy and free a window.
freeWindow :: (MonadSDLVideo m, SDLWindow a) => a -> m ()
freeWindow (toCWindow -> CWindow a) = liftIO $ finalizeForeignPtr a

-- | Get window flags from a given window.
windowFlags :: (MonadSDLVideo m, SDLWindow a) => a -> m [WindowFlags]
windowFlags (toCWindow -> w) = do
  n <- liftIO $ sDLGetWindowFlags w
  return $ filter (\f -> fromEnum f .&. n /= 0) [minBound..maxBound]

  where {#fun unsafe SDL_GetWindowFlags as ^
         { `CWindow' } -> `Int' #}

-- | Get internal window ID from a window.
getWindowID :: (MonadSDLVideo m, SDLWindow a) => a -> m WindowID
getWindowID (toCWindow -> w) = liftIO $ WindowID <$> sDLGetWindowID w
  where {#fun unsafe SDL_GetWindowID as ^
         { `CWindow' } -> `CUInt' id #}

-- | Try to restore a window from an internal ID. A window might be already destroyed.
getWindowFromID :: MonadSDLVideo m => WindowID -> m (Maybe SomeWindow)
getWindowFromID i = liftIO $ readMVar windows >>= maybe (return Nothing) deRefWeak . Map.lookup i

-- | Get a window position.
getWindowPosition :: (MonadSDLVideo m, SDLWindow a) => a -> m PosPoint
getWindowPosition (toCWindow -> w) = liftIO $ do
  (CInt x, CInt y) <- sDLGetWindowPosition w
  return $ P x y

  where {#fun unsafe SDL_GetWindowPosition as ^
         { `CWindow'
         , alloca- `CInt' peek*
         , alloca- `CInt' peek*
         } -> `()' #}

-- | Get a window size.
getWindowSize :: (MonadSDLVideo m, SDLWindow a) => a -> m Size
getWindowSize (toCWindow -> w) = liftIO $ do
  (CInt x, CInt y) <- sDLGetWindowSize w
  return $ P x y

  where {#fun unsafe SDL_GetWindowSize as ^
         { `CWindow'
         , alloca- `CInt' peek*
         , alloca- `CInt' peek*
         } -> `()' #}

-- | Set a window size.
setWindowSize :: (MonadSDLVideo m, SDLWindow a) => a -> Size -> m ()
setWindowSize (toCWindow -> win) (P w h) = liftIO $ sDLSetWindowSize win w h
  where {#fun SDL_SetWindowSize as ^
         { `CWindow', `Int32', `Int32' } -> `()' #}
