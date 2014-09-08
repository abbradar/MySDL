{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.SDL.Monad
       ( SDLT
       , withSDL
       ) where

import Control.Monad
import Data.Monoid
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Unsafe (unsafeUseAsCString)
import qualified Data.Text as T
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.C.String (CString)
import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Logger (MonadLogger(..),
                             LogLevel(..))
import System.Log.FastLogger (toLogStr, fromLogStr)
import FileLocation.LocationString (locationToString)

import Graphics.UI.SDL.Class

{#import Graphics.UI.SDL.Internal.Prim #}

#include <SDL2/SDL.h>

-- Main SDL monad transformer.
newtype SDLT m a = SDLT { runSDLT :: m a }
                 deriving (Functor, Applicative, Monad,
                           MonadFix)

instance MonadTrans SDLT where
  lift = SDLT

deriving instance MonadBase IO m => MonadBase IO (SDLT m)

instance MonadBaseControl IO m => MonadBaseControl IO (SDLT m) where
  newtype StM (SDLT m) a = StM {unStM :: StM m a}
  liftBaseWith = liftBaseThreaded SDLT runSDLT withSDL StM
  restoreM = SDLT . restoreM . unStM

instance MonadBase IO m => MonadSDL (SDLT m) where

-- Logging support
  
{#enum SDL_LogPriority as SDLLogPriority {underscoreToCase} deriving (Show, Eq) #}

{#enum SDL_LOG_CATEGORY_APPLICATION as SDLLogCategory {underscoreToCase} deriving (Show, Eq) #}

-- SDL_LogMessage uses varargs, so it can't be imported via c2hs.
foreign import ccall unsafe "SDL2/SDL_log.h SDL_LogMessage"
  logMessage :: Int -> Int -> CString -> CString -> IO ()

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
    liftBase $
      unsafeUseAsCString "%s" $ \format ->
      unsafeUseAsCString l $ \cmsg ->
      logMessage (fromEnum SdlLogCategoryApplication) (fromEnum $ priority lvl) format cmsg
    where priority LevelDebug = SdlLogPriorityDebug
          priority LevelWarn = SdlLogPriorityWarn
          priority LevelError = SdlLogPriorityError
          priority _ = SdlLogPriorityInfo

withSDL :: (MonadBaseControl IO m, MonadBase IO m) => SDLT m a -> m a
withSDL = unsafeWithSubSystem
          (liftBase $ do
              sdlCode "SDL_Init" $ sDLInit $ fromEnum mainSystem
              sDLLogSetPriority SdlLogCategoryApplication SdlLogPriorityVerbose
          )
          (liftBase {#call unsafe SDL_Quit as ^ #})
          mainSystem . runSDLT
  where mainSystem = NoParachute
        {#fun unsafe SDL_LogSetPriority as ^
         { `SDLLogCategory', `SDLLogPriority' } -> `()' #}
        -- TODO: Change 'Int' to 'SDLSubSystem' when issue #103 in c2hs is fixed.
        {#fun unsafe SDL_Init as ^ { `Int' } -> `Int' #}
