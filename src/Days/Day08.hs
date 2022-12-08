module Days.Day08 where
import           Data.Char       (digitToInt)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import qualified Util.Map        as Map
import Debug.Trace

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 21 8

type Input = Map Pos Int

type Output1 = Int
type Output2 = Int

type Pos = (Int, Int)

parser :: String -> Input
parser = Map.fromGrid . map (map digitToInt) . lines

part1 :: Input -> Output1
part1 m = Map.size $ Map.filterWithKey (\k v -> visible m k) m

visible :: Map Pos Int -> Pos -> Bool
visible m p@(x, y) = any (all (<tree)) [north, south, east, west]
    where
        tree  = m Map.! p
        north = Map.filterWithKey (\(x1, y1) _ -> x < x1 && y == y1) m
        south = Map.filterWithKey (\(x1, y1) _ -> x > x1 && y == y1) m
        east  = Map.filterWithKey (\(x1, y1) _ -> x == x1 && y < y1) m
        west  = Map.filterWithKey (\(x1, y1) _ -> x == x1 && y > y1) m

part2 :: Input -> Output2
part2 m = maximum $ Set.map (scenicScore m) $ Map.keysSet m

scenicScore :: Map Pos Int -> Pos -> Int
scenicScore m p@(x, y) = product $ map (length . (\(xs, ys) -> take 1 ys ++ xs) . span (<tree)) [north, south, east, west]
    where
        tree  = m Map.! p
        north = Map.elems $ Map.filterWithKey (\(x1, y1) _ -> x < x1 && y == y1) m
        south = reverse $ Map.elems $ Map.filterWithKey (\(x1, y1) _ -> x > x1 && y == y1) m
        east  = Map.elems $ Map.filterWithKey (\(x1, y1) _ -> x == x1 && y < y1) m
        west  = reverse $ Map.elems $ Map.filterWithKey (\(x1, y1) _ -> x == x1 && y > y1) m
