import           Control.Exception      (try, SomeException(..))
import           Data.Bifunctor         (second)
import           Test.Hspec

import qualified Days.Day01             as Day01 (testDay)
import qualified Days.Day02             as Day02 (testDay)
import qualified Days.Day03             as Day03 (testDay)
import qualified Days.Day04             as Day04 (testDay)
import qualified Days.Day05             as Day05 (testDay)
import qualified Days.Day06             as Day06 (testDay)
import qualified Days.Day07             as Day07 (testDay)
import qualified Days.Day08             as Day08 (testDay)
import qualified Days.Day09             as Day09 (testDay)
import qualified Days.Day10             as Day10 (testDay)
import qualified Days.Day11             as Day11 (testDay)
import qualified Days.Day12             as Day12 (testDay)
import qualified Days.Day13             as Day13 (testDay)
import qualified Days.Day14             as Day14 (testDay)
import qualified Days.Day15             as Day15 (testDay)
import qualified Days.Day16             as Day16 (testDay)
import qualified Days.Day17             as Day17 (testDay)
import qualified Days.Day18             as Day18 (testDay)
import qualified Days.Day19             as Day19 (testDay)
import qualified Days.Day20             as Day20 (testDay)
import qualified Days.Day21             as Day21 (testDay)
import qualified Days.Day22             as Day22 (testDay)
import qualified Days.Day23             as Day23 (testDay)
import qualified Days.Day24             as Day24 (testDay)
import qualified Days.Day25             as Day25 (testDay)
import Data.Either (rights)





testDays :: [(String -> String -> Spec, String)]
testDays = [
        (Day01.testDay, "input/Day01_test.txt"),
        (Day02.testDay, "input/Day02_test.txt"),
        (Day03.testDay, "input/Day03_test.txt"),
        (Day04.testDay, "input/Day04_test.txt"),
        (Day05.testDay, "input/Day05_test.txt"),
        (Day06.testDay, "input/Day06_test.txt"),
        (Day07.testDay, "input/Day07_test.txt"),
        (Day08.testDay, "input/Day08_test.txt"),
        (Day09.testDay, "input/Day09_test.txt"),
        (Day10.testDay, "input/Day10_test.txt"),
        (Day11.testDay, "input/Day11_test.txt"),
        (Day12.testDay, "input/Day12_test.txt"),
        (Day13.testDay, "input/Day13_test.txt"),
        (Day14.testDay, "input/Day14_test.txt"),
        (Day15.testDay, "input/Day15_test.txt"),
        (Day16.testDay, "input/Day16_test.txt"),
        (Day17.testDay, "input/Day17_test.txt"),
        (Day18.testDay, "input/Day18_test.txt"),
        (Day19.testDay, "input/Day19_test.txt"),
        (Day20.testDay, "input/Day20_test.txt"),
        (Day21.testDay, "input/Day21_test.txt"),
        (Day22.testDay, "input/Day22_test.txt"),
        (Day23.testDay, "input/Day23_test.txt"),
        (Day24.testDay, "input/Day24_test.txt"),
        (Day25.testDay, "input/Day25_test.txt")
    ]

main :: IO ()
main = do
    inputs <- (mapM (try . sequence . second (sequence . second readFile)) $ zip [1..] testDays) :: IO [Either SomeException (Int, (String -> String -> Spec, String))]
    hspec $ mapM_ (\(day, (td, filename)) -> td ("Day " <> show day) filename) $ rights inputs
