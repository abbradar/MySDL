module Control.Lens.Extra where

import Control.Applicative
import Data.Monoid
import Control.Lens

suitable :: (Choice p, Applicative f) => Getting Any a b -> (b -> Bool) -> Optic' p f a a
suitable l p = filtered (has $ l . filtered p)

hasn'tInside :: (Choice p, Applicative f) => Getting All a b -> Optic' p f a a
hasn'tInside l = filtered (hasn't l)

eqInside :: (Choice p, Applicative f, Eq b) => Getting Any a b -> b -> Optic' p f a a
eqInside l b = suitable l (== b)

hasInside :: (Choice p, Applicative f) => Getting Any a b -> Optic' p f a a
hasInside l = suitable l (const True)
