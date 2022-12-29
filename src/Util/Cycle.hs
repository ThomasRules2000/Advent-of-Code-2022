module Util.Cycle where

class (Eq a, Enum a, Bounded a) => Cycle a where
    next :: a -> a
    next x
        | x == maxBound = minBound
        | otherwise = succ x

    prev :: a -> a
    prev x
        | x == minBound = maxBound
        | otherwise = pred x