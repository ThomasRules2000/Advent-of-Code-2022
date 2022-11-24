module Util.Util where
import           Data.Bifunctor  (first, bimap)
import           Data.Char       (toLower, ord)
import           Data.Foldable   (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Matrix     (Matrix)
import qualified Data.Matrix     as Matrix
import           Data.Maybe      (fromJust, isJust)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace     (trace)

listToTuple :: [a] -> (a, a)
listToTuple [x,y] = (x, y)

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 [x, y, z] = (x, y, z)

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

-- binToDec :: [Bool] -> Int
-- binToDec = go . reverse
--     where
--         go :: [Bool] -> Int
--         go []     = 0
--         go (x:xs) = fromEnum x + 2 * go xs

-- binToDec :: [Bool] -> Int
-- binToDec = sum . map fst . filter snd . zip [2^x | x <-[0..]] . reverse

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

ppMatrix :: Matrix Bool -> String
ppMatrix matrix = unlines (Matrix.toLists (boolChar <$> matrix))
    where boolChar :: Bool -> Char
          boolChar True  = '█'
          boolChar False = ' '

traceTag :: Show a => String -> a -> a
traceTag s x = trace (s <> show x) x

sumTuples :: (Foldable t, Num a, Num b) => t (a, b) -> (a, b)
sumTuples = foldl' (uncurry bimap . bimap (+) (+)) (0, 0)