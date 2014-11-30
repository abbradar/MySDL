module Graphics.UI.SDL.Internal.Prim
       ( SDLBool(..)
       , sdlCall
       , sdlCode
       , sdlBool
       , sdlObject
       , SDLSubSystem(..)
       , unsafeWithSubSystem
       , withSubSystem
       , freeSDL
       ) where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Catch
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar
import Foreign.ForeignPtr.Safe (ForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.C.Types (CChar, CInt(..), CUInt(..))
import Foreign.C.String (peekCString)
import Control.Monad.IO.Class (MonadIO(..))
import Text.Printf (printf)

import Graphics.UI.SDL.Class

#define SDL_MAIN_HANDLED
#include <SDL2/SDL.h>

{#enum SDL_bool as SDLBool {underscoreToCase} deriving (Eq, Show) #}

sdlCall :: String -> IO a -> (a -> Bool) -> IO a
sdlCall str call test = do
  r <- call
  unless (test r) $ do
    -- Potential race condition (see haskell-cafe discussion)
    err <- {#call unsafe SDL_GetError as ^#} >>= peekCString
    --{#call unsafe SDL_ClearError as ^ #}
    fail $ str ++ ": " ++ err
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

{#enum define SDLSubSystem { SDL_INIT_TIMER as Timer
                           , SDL_INIT_AUDIO as Audio
                           , SDL_INIT_VIDEO as Video
                           , SDL_INIT_JOYSTICK as Joystick
                           , SDL_INIT_HAPTIC as Haptic
                           , SDL_INIT_GAMECONTROLLER as GameController
                           , SDL_INIT_EVENTS as Events
                           , SDL_INIT_NOPARACHUTE as NoParachute
                           } deriving (Show, Eq, Ord) #}

-- There is no way to check if SDL is initialized, so we must keep a global state. God will judge us.
-- Also, SDL's init routines are not thread-safe (and should not be, of course), so we should guard it against concurrent use.
sdlState :: MVar (Map SDLSubSystem Int)
{-# NOINLINE sdlState #-}
sdlState = unsafePerformIO $ do
  {#call unsafe SDL_SetMainReady as ^ #}
  newMVar Map.empty

modifyMVar_' :: (MonadIO m, MonadMask m) => MVar a -> (a -> m a) -> m ()
modifyMVar_' m io =
  mask $ \restore -> do
    a  <- liftIO $ takeMVar m
    a' <- restore (io a) `onException` (liftIO $ putMVar m a)
    liftIO $ putMVar m a'

-- This scary thing implements SDL subsystems refcounting, for both main SDL and subsystems.
unsafeWithSubSystem :: (MonadIO m, MonadMask m) => m () -> m () -> SDLSubSystem -> m a -> m a
unsafeWithSubSystem begin end sys =
  bracket_
    (modifyMVar_' sdlState $ \s ->
        if not $ sys `Map.member` s
        then do
          begin
          return $ Map.insert sys 1 s
        else return $ Map.adjust succ sys s
    )
    (modifyMVar_' sdlState $ \s -> do
        -- Error "should" be impossible if nothing else except withSubSystem tampers
        -- with sdlState.
        if (s Map.! sys) == 1
        then do
          end
          return $ Map.delete sys s
        else return $ Map.adjust pred sys s
    )

withSubSystem :: (MonadIO m, MonadMask m, MonadSDL m) => SDLSubSystem -> m a -> m a
withSubSystem sys = unsafeWithSubSystem
                    (liftIO $ sdlCode (printf "SDL_InitSubsystem(%s)" $ show sys) $
                     sDLInitSubSystem sys)
                    (liftIO $ sDLQuitSubSystem sys)
                    sys
  where {#fun unsafe SDL_InitSubSystem as ^ { `SDLSubSystem' } -> `Int' #}
        {#fun unsafe SDL_QuitSubSystem as ^ { `SDLSubSystem' } -> `()' #}

freeSDL :: Ptr a -> IO ()
freeSDL = {#call SDL_free as ^ #} . castPtr
