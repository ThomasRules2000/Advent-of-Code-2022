module Util.Map where
import           Data.Bifunctor  (first)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust, isJust)

(\/) :: Ord k => Map k v -> Map k v -> Map k v
(\/) = Map.union

(/\) :: Ord k => Map k v -> Map k v -> Map k v
(/\) = Map.intersection

containsKeys :: Ord k => Map k v -> [k] -> Bool
containsKeys m = all (`Map.member` m)

catMaybes :: Ord a => Map k (Maybe a) -> Map k a
catMaybes = Map.map fromJust . Map.filter isJust

fromGrid :: [[a]] -> Map (Int, Int) a
fromGrid = Map.fromAscList
         . concat
         . zipWith (\x -> map $ first (x,)) [0..]
         . map (zip [0..])
