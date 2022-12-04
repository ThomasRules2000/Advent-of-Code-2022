module Days.Day01Base where
import           Data.List      (groupBy, sort)
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 0 0

type Input = [[Int]]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (map read . tail) . groupBy (\x y -> y /= "") . ("":) . lines

part1 :: Input -> Output1
part1 = maximum . map sum

part2 :: Input -> Output2
part2 = sum . take 3 . reverse . sort . map sum
