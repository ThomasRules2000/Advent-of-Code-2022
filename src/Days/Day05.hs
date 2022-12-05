module Days.Day05 where
import           Data.Bifunctor  (bimap)
import           Data.Char       (isAlpha, isDigit, isSpace)
import           Data.List       (transpose)
import           Data.List.Split (splitOn)
import           Data.Vector     (Vector)
import qualified Data.Vector     as Vector
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.NoQuotes   (NoQuotes (..))
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 (NoQuotes "CMZ") (NoQuotes "MCD")

type Input = (Vector [Char], [Move])

type Output1 = NoQuotes
type Output2 = NoQuotes

data Move = Move {
    n  :: Int,
    t1 :: Int,
    t2 :: Int
} deriving (Eq, Ord, Show)

parser :: String -> Input
parser = bimap getTowers getMoves . listToTuple . splitOn "\n\n"
    where
        getTowers :: String -> Vector [Char]
        getTowers = Vector.fromList . map (init . dropWhile isSpace) . filter (any isAlpha) . transpose . lines

        getMoves :: String -> [Move]
        getMoves = map ((\[n,t1,t2] -> Move{t1=t1-1, t2=t2-1, ..}) . map read . filter (isDigit . head) . words) . lines

part1 :: Input -> Output1
part1 = getAns reverse

getAns :: ([Char] -> [Char]) -> Input -> NoQuotes
getAns f = NoQuotes . Vector.toList . fmap head . uncurry (foldl doMove)
    where doMove :: Vector [Char] -> Move -> Vector [Char]
          doMove tows Move{..} = tows Vector.// [(t1, newT1), (t2, f topT1 ++ tows Vector.! t2)]
               where (topT1, newT1) = splitAt n $ tows Vector.! t1

part2 :: Input -> Output2
part2 = getAns id
