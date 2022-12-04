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

parser :: String -> Input
parser = map (listToTuple . sort . map (listToTuple . map read . splitOn "-") . splitOn ",") . lines

part1 :: Input -> Output1
part1 = length . filter (uncurry (||) . uncurry (biliftA2 (==) (>=)))

part2 :: Input -> Output2
part2 = length . filter (uncurry (>=) . bimap snd fst)
