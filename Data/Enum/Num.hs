module Data.Enum.Num (
  fromEnum'
  ) where

fromEnum' :: (Enum a, Num b) => a -> b
fromEnum' = fromIntegral . fromEnum
