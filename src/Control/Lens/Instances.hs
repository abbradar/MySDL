{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Lens.Instances where

import Control.Applicative
import Control.Lens
import Data.Bits (Bits)
import Data.BitSet.Generic (BitSet)
import qualified Data.BitSet.Generic as B

type instance Index (BitSet c a) = a

instance (Bits c, Enum k) => Contains (BitSet c k) where
  contains k f s = f (B.member k s) <&> \b ->
    if b then B.insert k s else B.delete k s

type instance IxValue (BitSet c a) = ()
instance (Bits c, Enum k) => Ixed (BitSet c k) where
  ix k f m = if B.member k m
     then f () <&> \() -> B.insert k m
     else pure m

instance (Bits c, Enum k) => At (BitSet c k) where
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (B.delete k m)) mv
    Just () -> B.insert k m
    where mv = if B.member k m then Just () else Nothing
