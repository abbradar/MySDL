{-# LANGUAGE LambdaCase #-}

module Control.Monad.Maybe where

sequenceMaybe :: Monad m => [m (Maybe a)] -> m (Maybe a)
sequenceMaybe [] = return Nothing
sequenceMaybe (f:fs) = f >>= \case
  Nothing -> sequenceMaybe fs
  a -> return a
