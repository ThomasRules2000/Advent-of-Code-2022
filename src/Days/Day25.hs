module Days.Day25 where
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.NoQuotes

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 (NoQuotes "2=-1=0") $ NoQuotes "Merry Christmas"

type Input = [[Int]]

type Output1 = NoQuotes
type Output2 = NoQuotes

parser :: String -> Input
parser = map (map parseSnafu) . lines

parseSnafu :: Char -> Int
parseSnafu '0' = 0
parseSnafu '1' = 1
parseSnafu '2' = 2
parseSnafu '-' = -1
parseSnafu '=' = -2
parseSnafu c   = error $ "Invalid snafu char " ++ show c

part1 :: Input -> Output1
part1 = toSnafu . sum . map (foldl (\acc x -> 5 * acc + x) 0)

toSnafu :: Int -> NoQuotes
toSnafu = NoQuotes . map snafuChar . reverse . getNums
    where
        getNums x
            | x == 0 = []
            | dig >= 3 = dig - 5 : getNums (rest + 1)
            | otherwise = dig : getNums rest
            where (rest, dig) = x `divMod` 5
        snafuChar :: Int -> Char
        snafuChar 0    = '0'
        snafuChar 1    = '1'
        snafuChar 2    = '2'
        snafuChar (-1) = '-'
        snafuChar (-2) = '='
        snafuChar _    = error "Invalid number"

part2 :: Input -> Output2
part2 = const $ NoQuotes "Merry Christmas"
