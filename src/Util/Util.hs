module Util.Util where
import           Control.Monad  (filterM)
import           Data.Bifunctor (bimap)
import           Data.Char      (ord)
import           Data.Foldable  (foldl', maximumBy, minimumBy)
import           Data.Function  (on)
import           Debug.Trace    (trace)

listToTuple :: [a] -> (a, a)
listToTuple [x,y] = (x, y)

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 [x, y, z] = (x, y, z)

listToTuple4 :: [a] -> (a, a, a, a)
listToTuple4 [w, x, y, z] = (w, x, y, z)

trimap :: (a -> a') -> (b -> b') -> (c -> c') -> (a, b, c) -> (a', b', c')
trimap f g h (x, y, z) = (f x, g y, h z)

insertAfter :: Eq a => a -> [a] -> [a] -> [a]
insertAfter _ toInsert [] = toInsert
insertAfter after toInsert (l:ls)
  | l == after = l:(toInsert ++ ls)
  | otherwise  = l:insertAfter after toInsert ls

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

binToDec :: [Bool] -> Int
binToDec = foldl' (\acc b -> 2*acc + fromEnum b) 0

decToBin :: Int -> [Bool]
decToBin = reverse . decToBin'
    where decToBin' 0 = [False]
          decToBin' 1 = [True]
          decToBin' n = toEnum (n `mod` 2) : decToBin' (n `div` 2)

hexToBin :: String -> [Bool]
hexToBin s = replicate (4 - length i `mod` 4) False ++ i
    where i = decToBin $ hexToInt s

hexToInt :: String -> Int
hexToInt = foldl (\acc x -> 16*acc + x) 0 . map go
    where go c | '0' <= c && c <= '9' = ord c - ord '0'
               | 'a' <= c && c <= 'f' = ord c - ord 'a' + 10
               | 'A' <= c && c <= 'F' = ord c - ord 'A' + 10
               | otherwise = undefined

traceTag :: Show a => String -> a -> a
traceTag s x = trace (s <> show x) x

sumTuples :: (Foldable t, Num a, Num b) => t (a, b) -> (a, b)
sumTuples = foldl' (uncurry bimap . bimap (+) (+)) (0, 0)

takeWhileIncl :: (a -> Bool) -> [a] -> [a]
takeWhileIncl _ [] = []
takeWhileIncl p (x:xs)
    | p x = x : takeWhileIncl p xs
    | otherwise = [x]

takeEveryN :: Int -> [a] -> [a]
takeEveryN _ []     = []
takeEveryN n (x:xs) = x : takeEveryN n (drop (n-1) xs)

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True x  = Just x

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn f = maximumBy (compare `on` f)

minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
minimumOn f = minimumBy (compare `on` f)

powerSet :: [a] -> [[a]]
powerSet = filterM (const [True, False])
