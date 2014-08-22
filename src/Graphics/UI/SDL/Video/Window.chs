{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.UI.SDL.Video.Window
       ( Window
       , Position(..)
       , Point
       , Size
       , WindowFlags(..)
       , createWindow
       , freeWindow
       , windowFlags
       ) where

import Control.Monad
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Foreign.ForeignPtr.Safe (newForeignPtr_,
                                addForeignPtrFinalizer,
                                finalizeForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.C.Types (CChar(..), CInt(..), CUInt(..))
import Foreign.C.String (CString)
import Control.Exception.Lifted (mask_)
import Control.Monad.Base (liftBase)

import Data.Enum.Num
import Graphics.UI.SDL.Internal.Prim
import Graphics.UI.SDL.Video.Monad

{#import Graphics.UI.SDL.Video.Internal.Window #}

#include <SDL2/SDL_video.h>

type Window = CWindow

-- c2hs does not support FunPtr's yet...
foreign import ccall unsafe "SDL2/SDL_video.h &SDL_DestroyWindow"
  pDestroyWindow :: FunPtr (Ptr Window -> IO ())

{#enum define SDLWindowPos { SDL_WINDOWPOS_CENTERED as Centered'
                           , SDL_WINDOWPOS_UNDEFINED as Undefined'
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

{#enum SDL_WindowFlags as WindowFlags {underscoreToCase} deriving (Eq, Show, Bounded) #}

createWindow :: MonadSDLVideo m => ByteString -> Point -> Size -> [WindowFlags] -> m Window
createWindow name (x, y) (w, h) f = liftBase $ mask_ $ do
  let f' = foldr ((.|.) . fromEnum) 0 f
  a@(CWindow wh) <- unsafeUseAsCString name $ \cn ->
    sdlObject "SDL_CreateWindow" (\case CWindow a -> a) $ sDLCreateWindow cn x y w h f'
  addForeignPtrFinalizer pDestroyWindow wh
  return a

  where {#fun unsafe SDL_CreateWindow as ^
         { id `CString'
         , fromPosition `Position', fromPosition `Position'
         , `Int', `Int'
         , `Int' } -> `CWindow' #}

freeWindow :: MonadSDLVideo m => Window -> m ()
freeWindow (CWindow a) = liftBase $ finalizeForeignPtr a

windowFlags :: MonadSDLVideo m => Window -> m [WindowFlags]
windowFlags w = do
  n <- liftBase $ sDLGetWindowFlags w
  return $ filter (\f -> fromEnum f .&. n /= 0) [minBound..]

  where {#fun unsafe SDL_GetWindowFlags as ^
         { `CWindow' } -> `Int' #}
