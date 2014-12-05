{-|
Description: Common types used by library.
-}
module Graphics.UI.SDL.Types where

import Data.Int
import Control.Applicative

-- | Strict 2D point type.
data Point a = P !a !a
             deriving (Eq, Show)

-- | Convert tuple to 'Point'.
fromTuple2 :: (a, a) -> Point a
fromTuple2 = uncurry P

-- | Get tuple from 'Point'.

toTuple2 :: Point a -> (a, a)
toTuple2 (P a b) = (a, b)

instance Functor Point where
  fmap f (P a b) = P (f a) (f b)

instance Applicative Point where
  pure a = P a a
  (P f g) <*> (P a b) = P (f a) (g b)

instance Num a => Num (Point a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = liftA negate
  abs = liftA abs
  signum = liftA signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Point a) where
  (/) = liftA2 (/)
  recip = liftA recip
  fromRational = pure . fromRational

-- For joystick positions and such
type Position = Int32

type PosPoint = Point Position
type Size = Point Int32
