module Days.Day05 where
import           Data.Bifunctor
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Tuple.Extra (second3, third3, uncurry3)
import           Data.Vector      (Vector)
import qualified Data.Vector      as Vector
import qualified Program.RunDay   as R (runDay)
import qualified Program.TestDay  as T (testDay)
import           System.Clock     (TimeSpec)
import           Test.Hspec       (Spec)
import           Util.NoQuotes    (NoQuotes (..))
import           Util.Util
import Debug.Trace

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 (NoQuotes "CMZ") (NoQuotes "MCD")

type Input = (Vector [Char], [Move])

type Output1 = NoQuotes
type Output2 = NoQuotes

data Move = Move Int Int Int
    deriving (Eq, Ord, Show)

parser :: String -> Input
parser = bimap getTowers getMoves . listToTuple . splitOn "\n\n"
    where
        getTowers :: String -> Vector [Char]
        getTowers = Vector.fromList . map (init . dropWhile isSpace) . filter (any isAlpha) . transpose . lines

        getMoves :: String -> [Move]
        getMoves = map (uncurry3 Move . second3 (subtract 1) . third3 (subtract 1) . listToTuple3 . map read . filter (isDigit . head) . words) . lines

part1 :: Input -> Output1
part1 = getAns doMove
    where
        doMove :: Vector [Char] -> Move -> Vector [Char]
        doMove tows (Move n t1 t2) = tows Vector.// [(t1, newT1), (t2, reverse topT1 ++ tows Vector.! t2)]
            where (topT1, newT1) = splitAt n $ tows Vector.! t1

getAns :: (Vector [Char] -> Move -> Vector [Char]) -> Input -> NoQuotes
getAns doMove = NoQuotes . Vector.toList . fmap head . uncurry (foldl doMove)

part2 :: Input -> Output2
part2 = getAns doMove
    where
        doMove :: Vector [Char] -> Move -> Vector [Char]
        doMove tows (Move n t1 t2) = tows Vector.// [(t1, newT1), (t2, topT1 ++ tows Vector.! t2)]
            where (topT1, newT1) = splitAt n $ tows Vector.! t1
