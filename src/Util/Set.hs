module Util.Set where
import           Data.Maybe (fromJust, isJust)
import           Data.Set   (Set)
import qualified Data.Set   as Set

(\/) :: Ord a => Set a -> Set a -> Set a
(\/) = Set.union

(/\) :: Ord a => Set a -> Set a -> Set a
(/\) = Set.intersection

catMaybes :: Ord a => Set (Maybe a) -> Set a
catMaybes = Set.map fromJust . Set.filter isJust

intersections :: (Ord a, Foldable f) => f (Set a) -> Set a
intersections = foldl1 Set.intersection