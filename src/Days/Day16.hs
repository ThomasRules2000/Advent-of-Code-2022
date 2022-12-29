{-# LANGUAGE OverloadedStrings #-}
module Days.Day16 where
import           Control.Applicative              (many, (<|>))
import           Data.Attoparsec.ByteString.Char8 (Parser, decimal, endOfLine,
                                                   letter_ascii, sepBy, string)
import           Data.Bifunctor                   (bimap)
import           Data.IntMap.Strict               (IntMap)
import qualified Data.IntMap.Strict               as IntMap
import           Data.IntSet                      (IntSet)
import qualified Data.IntSet                      as IntSet
import           Data.List                        (sort)
import qualified Data.Map.Strict                  as Map
import           Data.Matrix                      (Matrix)
import qualified Data.Matrix                      as Matrix
import           Data.Monoid                      (Any (..))
import           Data.Tuple.Extra                 (both, dupe)
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as Vector
import qualified Program.RunDay                   as R (runDay)
import qualified Program.TestDay                  as T (testDay)
import           System.Clock                     (TimeSpec)
import           Test.Hspec                       (Spec)
import           Util.Util                        (powerSet)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 1651 1707

type Input = [(String, (Int, [String]))]

type Output1 = Int
type Output2 = Int

parser :: Parser Input
parser = sort <$> (valveParser `sepBy` endOfLine)

valveParser :: Parser (String, (Int, [String]))
valveParser = (,)
            <$> (string "Valve " *> many letter_ascii)
            <*> ((,) <$> (string " has flow rate=" *> decimal)
            <*> ((string "; tunnels lead to valves " <|> string "; tunnel leads to valve ")
                    *> many letter_ascii `sepBy` string ", "))

part1 :: Input -> Output1
part1 m = openValves 30 1 toOpen flows adjMatrix
    where
        (flows, adjMatrix) = numberNodes m
        toOpen = IntMap.keysSet flows

-- Seidel's algorithm for all pairs of shortest paths in O(V^omega log V), omega < 2.373
apd :: Matrix Int -> Matrix Int
apd a
    | and $ Matrix.mapPos (\(i,j) v -> i == j || v > 0) a = a
    | otherwise = fmap (2*) t - c
    where
        z = a ^ (2 :: Int)
        b = fmap (fromEnum . getAny) $ fmap (Any . (==1)) a <> Matrix.mapPos (\(i,j) v -> Any $ i /= j && v > 0) z
        t = apd b
        x = t * a
        degree = Vector.fromList $ map sum $ Matrix.toLists a
        c = Matrix.matrix (Matrix.nrows a) (Matrix.ncols a)
            $ \(i,j) -> fromEnum $ x Matrix.! (i,j) < t Matrix.! (i,j) * degree Vector.! (j-1)

getAdjMatrix :: Vector [Int] -> Matrix Int
getAdjMatrix v = apd $ Matrix.matrix (Vector.length v) (Vector.length v)
                $ \(i, j) -> fromEnum $ (j-1) `elem` (v Vector.! (i-1))

numberNodes :: [(String, (Int, [String]))] -> (IntMap Int, Matrix Int)
numberNodes m = bimap (IntMap.fromAscList . filter ((>0) . snd) . zip [1..])
                      (getAdjMatrix . Vector.fromList . fmap (map (nameNum Map.!)))
              $ unzip vals
    where
        (keys, vals) = unzip m
        nameNum = Map.fromAscList $ zip keys [0..]

openValves :: Int -> Int -> IntSet -> IntMap Int -> Matrix Int -> Int
openValves t currValve toOpen flows adjMatrix
    | t <= 0 = 0
    | IntSet.null toOpen = newPressure
    | otherwise = (newPressure+) $ maximum $ map nextValve $ IntSet.toList toOpen
    where
        newPressure = t * IntMap.findWithDefault 0 currValve flows
        nextValve :: Int -> Int
        nextValve newValve = openValves newT newValve newToOpen flows adjMatrix
            where
                newT = t - 1 - adjMatrix Matrix.! (currValve, newValve)
                newToOpen = IntSet.delete newValve toOpen

part2 :: Input -> Output2
part2 m = maximum $ map (uncurry (+) . both (\s -> openValves 26 1 s flows adjMatrix)) $ splitSet toOpen
    where
        (flows, adjMatrix) = numberNodes m
        toOpen = IntMap.keysSet flows

splitSet :: IntSet -> [(IntSet, IntSet)]
splitSet toOpen = map ((fmap (toOpen IntSet.\\) . dupe) . IntSet.fromAscList)
                $ filter ((<=IntSet.size toOpen `div` 2) . length)
                $ powerSet
                $ IntSet.toAscList toOpen
