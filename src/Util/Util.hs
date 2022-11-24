module Util.Util where
import           Data.Bifunctor  (first, bimap)
import           Data.Char       (toLower)
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
decToBin 0 = [False]
decToBin 1 = [True]
decToBin n = toEnum (n `mod` 2) : decToBin (n `div` 2)

hexToBin :: String -> [Bool]
hexToBin = concatMap (go . toLower)
  where
    go '0' = [False, False, False, False]
    go '1' = [False, False, False, True ]
    go '2' = [False, False, True,  False]
    go '3' = [False, False, True,  True ]
    go '4' = [False, True,  False, False]
    go '5' = [False, True,  False, True ]
    go '6' = [False, True,  True,  False]
    go '7' = [False, True,  True,  True ]
    go '8' = [True,  False, False, False]
    go '9' = [True,  False, False, True ]
    go 'a' = [True,  False, True,  False]
    go 'b' = [True,  False, True,  True ]
    go 'c' = [True,  True,  False, False]
    go 'd' = [True,  True,  False, True ]
    go 'e' = [True,  True,  True,  False]
    go 'f' = [True,  True,  True,  True ]
    go  c  = []

boolChar :: Bool -> Char
boolChar True  = '█'
boolChar False = ' '

ppMatrix :: Matrix Bool -> String
ppMatrix matrix = unlines (Matrix.toLists (boolChar <$> matrix))

traceTag :: Show a => String -> a -> a
traceTag s x = trace (s <> show x) x

sumTuples :: (Foldable t, Num a, Num b) => t (a, b) -> (a, b)
sumTuples = foldl' (uncurry bimap . bimap (+) (+)) (0, 0)