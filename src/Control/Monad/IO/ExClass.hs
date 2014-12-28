{-|
Description: 7.10 with FAM-proposal f*cking when?
-}

module Control.Monad.IO.ExClass
       ( module Class
       , MonadIO'
       ) where

import Control.Monad.IO.Class as Class
import Control.Applicative

type MonadIO' m = (MonadIO m, Applicative m)
