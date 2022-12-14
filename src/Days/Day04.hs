module Days.Day04 where
import           Data.Biapplicative (biliftA2)
import           Data.Bifunctor     (bimap)
import           Data.List          (sort)
import           Data.List.Split    (splitOn)
import qualified Program.RunDay     as R (runDay)
import qualified Program.TestDay    as T (testDay)
import           System.Clock       (TimeSpec)
import           Test.Hspec         (Spec)
import           Util.Util          (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 2 4

type Input = [((Int, Int), (Int, Int))]

type Output1 = Int
type Output2 = Int

-- We ensure that the range with the lower start appears first to simplify some logic later
parser :: String -> Input
parser = map (listToTuple . sort . map (listToTuple . map read . splitOn "-") . splitOn ",") . lines

-- r1@(x1, y1) r2@(x2, y2)
-- r2 contains r1 if x1 >= x2 && y1 <= y2, but as x1 <= x2 this simplifies to x1 == x2 && y1 <= y2
-- r1 contains r2 if x1 <= x2 && y1 >= y2, but as x1 <= x2 this simplifies to y1 >= y2
-- => one range contains the other = (x1 == x2 && y1 <= y2) || y1 >= y2
-- = (x1 == x2 || y1 >= y2) && (y1 <= y2 || y1 >= y2)
-- = x1 == x2 || y2 >= y2
part1 :: Input -> Output1
part1 = length . filter (uncurry (||) . uncurry (biliftA2 (==) (>=)))

-- Ranges overlap if the top of the first is greater than or equal to the bottom of the second
part2 :: Input -> Output2
part2 = length . filter (uncurry (>=) . bimap snd fst)
