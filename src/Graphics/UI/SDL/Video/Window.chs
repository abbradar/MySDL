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

import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Foreign.ForeignPtr
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
import Control.Monad.IO.ExClass

import Data.Enum.Num
import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Types

{#import Graphics.UI.SDL.Video.Internal.Window #}

#include <SDL2/SDL_video.h>

-- c2hs does not support FunPtr's yet...
foreign import ccall unsafe "SDL2/SDL_video.h &SDL_DestroyWindow"
  pDestroyWindow :: FinalizerPtr ()

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
createWindow :: MonadIO' m => ByteString -> V2 PositionHint -> Size -> [WindowFlags] -> m SomeWindow
createWindow name (V2 x y) (V2 w h) fs = do
  cw <- liftIO $ mask_ $ do
    let f = foldr ((.|.) . fromEnum') 0 fs
    wh <- unsafeUseAsCString name $ \cn ->
      sdlObject "SDL_CreateWindow" id $ sDLCreateWindow cn x y w h f
    addForeignPtrFinalizer pDestroyWindow wh
    return wh
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
freeWindow :: (MonadIO' m, SDLWindow a) => a -> m ()
freeWindow (toCWindow -> a) = liftIO $ finalizeForeignPtr a

-- | Get window flags from a given window.
windowFlags :: (MonadIO' m, SDLWindow a) => a -> m [WindowFlags]
windowFlags (toCWindow -> w) = do
  n <- liftIO $ sDLGetWindowFlags w
  return $ filter (\f -> fromEnum f .&. n /= 0) [minBound..maxBound]

  where {#fun unsafe SDL_GetWindowFlags as ^
         { `CWindow' } -> `Int' #}

-- | Get internal window ID from a window.
getWindowID :: (MonadIO' m, SDLWindow a) => a -> m WindowID
getWindowID (toCWindow -> w) = liftIO $ WindowID <$> sDLGetWindowID w
  where {#fun unsafe SDL_GetWindowID as ^
         { `CWindow' } -> `CUInt' id #}

-- | Try to restore a window from an internal ID. A window might be already destroyed.
getWindowFromID :: MonadIO' m => WindowID -> m (Maybe SomeWindow)
getWindowFromID i = liftIO $ readMVar windows >>= maybe (return Nothing) deRefWeak . Map.lookup i

-- | Get a window position.
getWindowPosition :: (MonadIO' m, SDLWindow a) => a -> m PosPoint
getWindowPosition (toCWindow -> w) = liftIO $ do
  (CInt x, CInt y) <- sDLGetWindowPosition w
  return $ V2 x y

  where {#fun unsafe SDL_GetWindowPosition as ^
         { `CWindow'
         , alloca- `CInt' peek*
         , alloca- `CInt' peek*
         } -> `()' #}

-- | Get a window size.
getWindowSize :: (MonadIO' m, SDLWindow a) => a -> m Size
getWindowSize (toCWindow -> w) = liftIO $ do
  (CInt x, CInt y) <- sDLGetWindowSize w
  return $ V2 x y

  where {#fun unsafe SDL_GetWindowSize as ^
         { `CWindow'
         , alloca- `CInt' peek*
         , alloca- `CInt' peek*
         } -> `()' #}

-- | Set a window size.
setWindowSize :: (MonadIO' m, SDLWindow a) => a -> Size -> m ()
setWindowSize (toCWindow -> win) (V2 w h) = liftIO $ sDLSetWindowSize win w h
  where {#fun SDL_SetWindowSize as ^
         { `CWindow', `Int32', `Int32' } -> `()' #}
