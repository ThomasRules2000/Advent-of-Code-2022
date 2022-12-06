module Days.Day06 where
import           Data.List       (tails)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 7 19

type Input = String

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = id

part1 :: Input -> Output1
part1 = solve 4

part2 :: Input -> Output2
part2 = solve 14

solve :: Int -> String -> Int
solve n = (+n) . length . takeWhile ((<n) . Set.size) . map (Set.fromList . take n) . tails
