module Util.Pair where

data Pair a = Pair a a deriving (Eq, Ord, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  Pair f g <*> Pair x y = Pair (f x) (g y)

fromTuple :: (a, a) -> Pair a
fromTuple (x,y) = Pair x y

fromList :: [a] -> Pair a
fromList [x,y] = Pair x y
fromList _     = error "List must contain exactly 2 elements"

first :: Pair a -> a
first (Pair x _) = x

second :: Pair a -> a
second (Pair _ y) = y
