{-|
Description: Helpful lenses, especially for SDL wires.
-}
module Control.Lens.Extra where

import Control.Applicative
import Data.Monoid
import Control.Lens

-- | Filters things that has something to be got though @l@ which satisfies @p@.
suitable :: (Choice p, Applicative f) => Getting Any a b -> (b -> Bool) -> Optic' p f a a
suitable l p = filtered (has $ l . filtered p)

-- | Like 'suitable', but just checks that given 'Traversal' returns nothing.
hasn'tInside :: (Choice p, Applicative f) => Getting All a b -> Optic' p f a a
hasn'tInside = filtered . hasn't

-- | Like 'suitable', but just tests for equality of @b@.
eqInside :: (Choice p, Applicative f, Eq b) => Getting Any a b -> b -> Optic' p f a a
eqInside l b = suitable l (== b)

-- | Opposite of 'hasn'tInside'.
hasInside :: (Choice p, Applicative f) => Getting Any a b -> Optic' p f a a
hasInside l = suitable l (const True)
