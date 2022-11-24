module Util.BinTree where

data BinTree a = Node (BinTree a) (BinTree a)
               | Leaf a
               deriving (Eq, Ord, Functor, Foldable)

instance Show a => Show (BinTree a) where
    show (Leaf x) = show x 
    show (Node x y) = concat ["[", show x, ",", show y, "]"] 

-- Get the maximum depth of the tree
maxDepth :: BinTree a -> Int
maxDepth (Leaf _)   = 0
maxDepth (Node l r) = 1 + max (maxDepth l) (maxDepth r)

-- Apply a function to the leftmost node in the tree
applyLeft :: (a -> a) -> BinTree a -> BinTree a
applyLeft f (Leaf x)   = Leaf $ f x
applyLeft f (Node l r) = Node (applyLeft f l) r

-- Apply a function to the rightmost node in the tree
applyRight :: (a -> a) -> BinTree a -> BinTree a
applyRight f (Leaf x)   = Leaf $ f x
applyRight f (Node l r) = Node l $ applyRight f r