{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.SDL.Internal.Unsafe where

import Control.Monad (liftM)
import Control.Applicative (Applicative)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadTransControl(..),
                                    MonadBaseControl(..),
                                    ComposeSt,
                                    defaultLiftBaseWith,
                                    defaultRestoreM)

import Graphics.UI.SDL.Class

newtype UnsafeSDLT m a = UnsafeSDLT { unsafeRunSDLT :: m a }
                       deriving (Functor, Applicative, Monad,
                                 MonadFix)

instance MonadTrans UnsafeSDLT where
  lift = UnsafeSDLT

deriving instance MonadBase IO m => MonadBase IO (UnsafeSDLT m)

instance MonadTransControl UnsafeSDLT where
     newtype StT UnsafeSDLT a = StT {unStT :: a}
     liftWith f = UnsafeSDLT $ f $ unsafeRunSDLT . liftM StT
     restoreT = UnsafeSDLT . liftM unStT

instance MonadBaseControl IO m => MonadBaseControl IO (UnsafeSDLT m) where
  newtype StM (UnsafeSDLT m) a = StM {unStM :: ComposeSt UnsafeSDLT m a}
  liftBaseWith = defaultLiftBaseWith StM
  restoreM = defaultRestoreM unStM

instance MonadBase IO m => MonadSDL (UnsafeSDLT m) where
instance MonadBase IO m => MonadSDLAudio (UnsafeSDLT m) where
instance MonadBase IO m => MonadSDLEvents (UnsafeSDLT m) where
instance MonadBase IO m => MonadSDLTimer (UnsafeSDLT m) where
instance MonadBase IO m => MonadSDLVideo (UnsafeSDLT m) where
