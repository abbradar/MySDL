{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.SDL.Video.Window (
  Window,
  Position(..),
  Point,
  Size,
  WindowFlags(..),
  createWindow,
  destroyWindow,
  windowFlags
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Base (MonadBase(..))
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Foreign (ForeignPtr, newForeignPtr, withForeignPtr, finalizeForeignPtr)
import Foreign.Ptr (FunPtr, Ptr, castPtr)
import Foreign.C.Types (CChar(..), CInt(..), CUInt(..))
import Foreign.C.String (CString)
import Control.Exception (mask_)

import Data.Enum.Num
import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Video

#include <SDL2/SDL_video.h>

data CWindow
newtype Window = Window { unWindow :: ForeignPtr CWindow }

foreign import ccall unsafe "SDL2/SDL_video.h &SDL_DestroyWindow"
  pDestroyWindow :: FunPtr (Ptr CWindow -> IO ())

{#enum define SDLWindowPos {
    SDL_WINDOWPOS_CENTERED as Centered',
    SDL_WINDOWPOS_UNDEFINED as Undefined'
    } #}

data Position = Pos Int
              | Centered
              | Undefined
                deriving (Show, Eq)

fromPosition :: Position -> CInt
fromPosition (Pos x) = fromIntegral x
fromPosition Centered = fromEnum' Centered'
fromPosition Undefined = fromEnum' Undefined'

type Point = (Position, Position)

type Size = (Int, Int)

{#enum SDL_WindowFlags as WindowFlags {
    SDL_WINDOW_FULLSCREEN as Fullscreen,
    SDL_WINDOW_FULLSCREEN_DESKTOP as FullscreenDesktop,
    SDL_WINDOW_OPENGL as OpenGL,
    SDL_WINDOW_SHOWN as Shown,
    SDL_WINDOW_HIDDEN as Hidden,
    SDL_WINDOW_BORDERLESS as Borderless,
    SDL_WINDOW_RESIZABLE as Resizable,
    SDL_WINDOW_MINIMIZED as Minimized,
    SDL_WINDOW_MAXIMIXED as Maximized,
    SDL_WINDOW_INPUT_GRABBED as InputGrabbed,
    SDL_WINDOW_INPUT_FOCUS as InputFocus,
    SDL_WINDOW_ALLOW_HIGHDPI as HighDPI
    } deriving (Eq, Show, Bounded) #}

createWindow :: MonadSDLVideo m => ByteString -> Point -> Size -> [WindowFlags] -> m Window
createWindow name (x, y) (w, h) f = liftBase $ mask_ $ do
  let f' = foldr ((.|.) . fromEnum) 0 f
  ptr <- B.useAsCString name $ \cn -> do
    sdlPtr "SDL_CreateWindow" $ sDLCreateWindow cn x y w h f'
  Window <$> newForeignPtr pDestroyWindow ptr
  where {#fun unsafe SDL_CreateWindow as ^
         { id `CString'
         , fromPosition `Position', fromPosition `Position'
         , `Int', `Int'
         , `Int' } -> `Ptr CWindow' castPtr #}

destroyWindow :: MonadBase IO m => Window -> m ()
destroyWindow = liftBase . finalizeForeignPtr . unWindow

windowFlags :: MonadBase IO m => Window -> m [WindowFlags]
windowFlags w = do
  n <- liftBase $ withForeignPtr (unWindow w) sDLGetWindowFlags
  return $ filter (\f -> fromEnum f .&. n /= 0) [minBound..]
  where {#fun unsafe SDL_GetWindowFlags as ^
         { castPtr `Ptr CWindow' } -> `Int' #}
