module FRP.Netwire.Extra where

import Prelude hiding ((.))
import Control.Wire
import Control.Wire.Unsafe.Event (Event(..))

mapE :: (a -> b) -> Wire s e m (Event a) (Event b)
mapE f = mkSF_ $ \case
  NoEvent -> NoEvent
  Event a -> Event $ f a

emptyE :: Wire s e m (Event a) (Event ())
emptyE = mapE $ const ()

(<!>) :: Monad m => Wire s e m a (Event e1) -> Wire s e m a (Event e2) -> Wire s e m a (Event ())
a <!> b = emptyE . a <> emptyE . b

infixl 5 <!>
