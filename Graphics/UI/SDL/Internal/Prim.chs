{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.UI.SDL.Internal.Prim (
  sdlCall,
  sdlCode,
  sdlPtr,
  SDLSubSystem(Timer, Audio, Video, Joystick, Haptic, GameController, Events),
  withSubSystem,
  SDLT,
  MonadSDL,
  withSDL
  ) where

import Control.Monad
import Data.Monoid
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Unsafe (unsafeUseAsCString)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception.Lifted
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CChar, CInt(..), CUInt(..))
import Foreign.C.String (CString, newCString, peekCString)
import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadTransControl(..),
                                    MonadBaseControl(..),
                                    ComposeSt,
                                    defaultLiftBaseWith,
                                    defaultRestoreM)
import Control.Monad.Logger (MonadLogger(..),
                             LogLevel(..))
import System.Log.FastLogger (toLogStr, fromLogStr)
import FileLocation.LocationString (locationToString)

import Graphics.UI.SDL.Internal.Class

#include <SDL2/SDL.h>

-- This is not thread-safe really; we don't do any locking of
-- SDL calls optimistically. But we are using 'IO a' instead of 'a'
-- to have a way to make such locking in future.
sdlCall :: String -> IO a -> (a -> Bool) -> IO a
sdlCall str call test = do
  r <- call
  unless (test r) $ do
    err <- {#call unsafe SDL_GetError as ^#} >>= peekCString
    --{#call unsafe SDL_ClearError as ^ #}
    fail $ str ++ ": " ++ err
  return r

sdlCode :: String -> IO Int -> IO ()
sdlCode str call = sdlCall str call (== 0) >> return ()

sdlPtr :: String -> IO (Ptr a) -> IO (Ptr a)
sdlPtr str call = sdlCall str call (/= nullPtr)

{#enum define SDLSubSystem {
    SDL_INIT_TIMER as Timer,
    SDL_INIT_AUDIO as Audio,
    SDL_INIT_VIDEO as Video,
    SDL_INIT_JOYSTICK as Joystick,
    SDL_INIT_HAPTIC as Haptic,
    SDL_INIT_GAMECONTROLLER as GameController,
    SDL_INIT_EVENTS as Events,
    SDL_INIT_NOPARACHUTE as NoParachute
    } deriving (Show, Eq, Ord) #}

-- There is no way to check if SDL is initialized, so we must keep a global state. God will judge us.
-- Also, SDL's init routines are not thread-safe (and should not be, of course), so we should guard it against concurrent use.
sdlState :: MVar (Map SDLSubSystem Int)
{-# NOINLINE sdlState #-}
sdlState = unsafePerformIO $ newMVar Map.empty

-- This scary thing implements SDL subsystems refcounting, for both main SDL and subsystems.
withSubSystem' :: (MonadBaseControl IO m, MonadBase IO m) => m () -> m () -> SDLSubSystem -> m a -> m a
withSubSystem' begin end sys =
  bracket_
    (bracketOnError
     (liftBase $ takeMVar sdlState)
     (liftBase . putMVar sdlState)
     (\s -> if not $ sys `Map.member` s
            then do
              begin
              liftBase $ putMVar sdlState $ Map.insert sys 1 s
            else liftBase $ putMVar sdlState $ Map.adjust succ sys s)
    )
    (bracketOnError
        (liftBase $ takeMVar sdlState)
        (liftBase . putMVar sdlState)
        (\s -> do
            -- Error "should" be impossible if nothing else except withSubSystem tampers
            -- with sdlState.
            let n = s Map.! sys
            if n == 1
              then
              do
                end
                liftBase $ putMVar sdlState $ Map.delete sys s
              else liftBase $ putMVar sdlState $ Map.adjust pred sys s
        )
    )

-- Main SDL monad transformer.
newtype SDLT m a = SDLT { runSDLT :: m a }
              deriving (Functor, Applicative, Monad,
                        MonadFix)

instance MonadTrans SDLT where
  lift = SDLT

instance MonadBase IO m => MonadBase IO (SDLT m) where
  liftBase = lift . liftBase

instance MonadTransControl SDLT where
     newtype StT SDLT a = StT {unStT :: a}
     liftWith f = SDLT $ f $ runSDLT . liftM StT
     restoreT = SDLT . liftM unStT

instance MonadBaseControl IO m => MonadBaseControl IO (SDLT m) where
  newtype StM (SDLT m) a = StM {unStM :: ComposeSt SDLT m a}
  liftBaseWith = defaultLiftBaseWith StM
  restoreM = defaultRestoreM unStM

instance MonadBase IO m => MonadSDL (SDLT m) where

-- Logging support
  
{#enum SDL_LogPriority as SDLLogPriority {underscoreToCase} deriving (Show, Eq) #}

{#enum SDL_LOG_CATEGORY_APPLICATION as SDLLogCategory {underscoreToCase} deriving (Show, Eq) #}

foreign import ccall unsafe "SDL2/SDL_log.h SDL_LogMessage"
  logMessage :: Int -> Int -> CString -> CString -> IO ()

-- We use global constant here for 'logMessage' "format" parameter
loggerFormat :: CString
{-# NOINLINE loggerFormat #-}
loggerFormat = unsafePerformIO $ newCString "%s"

instance (MonadBase IO m) => MonadLogger (SDLT m) where
  monadLoggerLog loc src lvl msg = do
    let l = fromLogStr $
            (case lvl of
                LevelOther t -> "(" `mappend` toLogStr t `mappend` ") "
                _ -> mempty
            ) `mappend`
            (if T.null src
             then mempty
             else "[" `mappend` toLogStr src `mappend` "] "
            ) `mappend` toLogStr msg `mappend` " @("
            `mappend` toLogStr (S8.pack $ locationToString loc)
            `mappend` ")\n\0"
    liftBase $ unsafeUseAsCString l $
     logMessage (fromEnum SdlLogCategoryApplication)
     (fromEnum $ priority lvl) loggerFormat
    where priority LevelDebug = SdlLogPriorityDebug
          priority LevelWarn = SdlLogPriorityWarn
          priority LevelError = SdlLogPriorityError
          priority _ = SdlLogPriorityInfo

withSubSystem :: (MonadBaseControl IO m, MonadBase IO m, MonadSDL m) => SDLSubSystem -> m a -> m a
withSubSystem sys = withSubSystem'
                    (liftBase $ sdlCode ("SDL_InitSubsystem(" ++ show sys ++ ")") $
                     sDLInitSubSystem sys)
                    (liftBase $ sDLQuitSubSystem sys)
                    sys
  where {#fun unsafe SDL_InitSubSystem as ^ { `SDLSubSystem' } -> `Int' #}
        {#fun unsafe SDL_QuitSubSystem as ^ { `SDLSubSystem' } -> `()' #}

withSDL :: (MonadBaseControl IO m, MonadBase IO m) => SDLT m a -> m a
withSDL = withSubSystem'
          (liftBase $ do
              sdlCode "SDL_Init" $ sDLInit mainSystem
              sDLLogSetPriority SdlLogCategoryApplication SdlLogPriorityVerbose
          )
          (liftBase {#call unsafe SDL_Quit as ^ #})
          mainSystem . runSDLT
  where mainSystem = NoParachute
        {#fun unsafe SDL_LogSetPriority as ^
         { `SDLLogCategory', `SDLLogPriority' } -> `()' #}
        {#fun unsafe SDL_Init as ^ { `SDLSubSystem' } -> `Int' #}
