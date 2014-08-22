module Data.Enum.Num  where

fromEnum' :: (Enum a, Num b) => a -> b
fromEnum' = fromIntegral . fromEnum

toEnum' :: (Enum a, Integral b) => b -> a
toEnum' = toEnum . fromIntegral
