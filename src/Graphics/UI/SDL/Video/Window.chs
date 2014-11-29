module Graphics.UI.SDL.Video.Window
       ( Window
       , GLWindow
       , SomeWindow
       , SDLWindow(toSomeWindow)
       , PositionHint(..)
       , PositionHints
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
       , unsafeSetWindowSize
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
import Control.Exception.Lifted (mask_)
import Control.Monad.Base (liftBase)
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative ((<$>))
import Data.Word
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import System.Mem.Weak (Weak, mkWeak, deRefWeak)

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

data PositionHint = Abs !Int32
                  | Centered
                  | Undefined
                deriving (Show, Eq)

fromPositionHint :: PositionHint -> CInt
fromPositionHint (Abs x) = CInt x
fromPositionHint Centered = fromEnum' Centered'
fromPositionHint Undefined = fromEnum' Undefined'

type PositionHints = Point PositionHint

{#enum SDL_WindowFlags as WindowFlags {underscoreToCase} deriving (Eq, Show, Bounded) #}

-- There is no "refcount" field in SDL_Window and we can deceive GC by using getWindowFromID.
-- Here goes another global state.
windows :: MVar (Map WindowID (Weak SomeWindow))
{-# NOINLINE windows #-}
windows = unsafePerformIO $ newMVar Map.empty

createWindow :: MonadSDLVideo m => ByteString -> PositionHints -> Size -> [WindowFlags] -> m SomeWindow
createWindow name (P x y) (P w h) fs = do
  cw <- liftBase $ mask_ $ do
    let f = foldr ((.|.) . fromEnum') 0 fs
    cw@(CWindow wh) <- unsafeUseAsCString name $ \cn ->
      sdlObject "SDL_CreateWindow" (\case CWindow a -> a) $ sDLCreateWindow cn x y w h f
    addForeignPtrFinalizer pDestroyWindow wh
    return cw
  let win = (if SdlWindowOpengl `elem` fs then toSomeWindow . GLWindow else toSomeWindow . Window) cw
  wid <- getWindowID win
  liftBase $ do
    p <- mkWeak cw win $ Just $ modifyMVar_ windows $ return . Map.delete wid
    modifyMVar_ windows $ return . Map.insert wid p
    return win

  where {#fun unsafe SDL_CreateWindow as ^
         { id `CString'
         , fromPositionHint `PositionHint', fromPositionHint `PositionHint'
         , `Int32', `Int32'
         , `Word32' } -> `CWindow' #}

freeWindow :: (MonadSDLVideo m, SDLWindow a) => a -> m ()
freeWindow (toCWindow -> CWindow a) = liftBase $ finalizeForeignPtr a

windowFlags :: (MonadSDLVideo m, SDLWindow a) => a -> m [WindowFlags]
windowFlags (toCWindow -> w) = do
  n <- liftBase $ sDLGetWindowFlags w
  return $ filter (\f -> fromEnum f .&. n /= 0) [minBound..maxBound]

  where {#fun unsafe SDL_GetWindowFlags as ^
         { `CWindow' } -> `Int' #}

getWindowID :: (MonadSDLVideo m, SDLWindow a) => a -> m WindowID
getWindowID (toCWindow -> w) = liftBase $ WindowID <$> sDLGetWindowID w
  where {#fun unsafe SDL_GetWindowID as ^
         { `CWindow' } -> `CUInt' id #}

getWindowFromID :: MonadSDLVideo m => WindowID -> m (Maybe SomeWindow)
getWindowFromID i = liftBase $ readMVar windows >>= maybe (return Nothing) deRefWeak . Map.lookup i

getWindowPosition :: (MonadSDLVideo m, SDLWindow a) => a -> m PosPoint
getWindowPosition (toCWindow -> w) = liftBase $ do
  (CInt x, CInt y) <- sDLGetWindowPosition w
  return $ P x y

  where {#fun unsafe SDL_GetWindowPosition as ^
         { `CWindow'
         , alloca- `CInt' peek*
         , alloca- `CInt' peek*
         } -> `()' #}

getWindowSize :: (MonadSDLVideo m, SDLWindow a) => a -> m Size
getWindowSize (toCWindow -> w) = liftBase $ do
  (CInt x, CInt y) <- sDLGetWindowSize w
  return $ P x y

  where {#fun unsafe SDL_GetWindowSize as ^
         { `CWindow'
         , alloca- `CInt' peek*
         , alloca- `CInt' peek*
         } -> `()' #}

-- SDL_SetWindowSize calls SDL_PushEvent which can callback into Haskell code.
setWindowSize' :: (MonadSDLVideo m, SDLWindow a) => (CWindow -> Int32 -> Int32 -> IO ()) -> a -> Size -> m ()
setWindowSize' call (toCWindow -> win) (P w h) = liftBase $ call win w h

setWindowSize :: (MonadSDLVideo m, SDLWindow a) => a -> Size -> m ()
setWindowSize = setWindowSize' sDLSetWindowSize
  where {#fun SDL_SetWindowSize as ^
         { `CWindow', `Int32', `Int32' } -> `()' #}

unsafeSetWindowSize :: (MonadSDLVideo m, SDLWindow a) => a -> Size -> m ()
unsafeSetWindowSize = setWindowSize' sDLSetWindowSizeUnsafe
  where {#fun unsafe SDL_SetWindowSize as sDLSetWindowSizeUnsafe
         { `CWindow', `Int32', `Int32' } -> `()' #}
