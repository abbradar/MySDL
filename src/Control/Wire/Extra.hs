{-|
Description: Extra helpful wires.
-}

module Control.Wire.Extra where

import Prelude hiding ((.))
import Control.Wire

-- | Create a pure wire on 'Event' from a function.
mapE :: (a -> b) -> Wire s e m (Event a) (Event b)
mapE = mkSF_ . fmap

-- | Clear additional data from 'Event'
emptyE :: Wire s e m (Event a) (Event ())
emptyE = mapE $ const ()

-- | Sum any 'Event' wires removing additional data from them.
(<!>) :: Monad m => Wire s e m a (Event e1) -> Wire s e m a (Event e2) -> Wire s e m a (Event ())
a <!> b = emptyE . a <> emptyE . b

infixl 5 <!>
