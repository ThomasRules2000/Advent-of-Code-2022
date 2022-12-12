module Days.Day12 where
import           Data.Bifunctor  (bimap)
import           Data.Char       (ord)
import           Data.Heap       (MinPrioHeap)
import qualified Data.Heap       as Heap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Tuple.All  (uncurryN)
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import qualified Util.Map        as Map

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 31 29

type Pos = (Int, Int)

type Input = (Map Pos Int, Pos, Pos)

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser inp = (getElevation <$> grid, start, end)
    where
        grid = Map.fromGrid $ lines inp
        start = head $ Map.keys $ Map.filter (=='S') grid
        end = head $ Map.keys $ Map.filter (=='E') grid

getElevation :: Char -> Int
getElevation 'S' = 0
getElevation 'E' = 25
getElevation c
    | 'a' <= c && c <= 'z' = ord c - ord 'a'
    | otherwise = error $ "Invalid Character " <> show c

part1 :: Input -> Output1
part1 = uncurryN (dijkstra Set.empty) . bimap Set.singleton (Heap.singleton . (0,))

dijkstra :: Set Pos -> Map Pos Int -> Set Pos -> MinPrioHeap Int Pos  -> Int
dijkstra visited elevations starts queue
    | curr `Set.member` starts = cost
    | curr `Set.member` visited = dijkstra visited elevations starts rest
    | otherwise = dijkstra (Set.insert curr visited) elevations starts (foldr (\p h -> Heap.insert (cost+1, p) h) rest next)
    where
        Just ((cost, curr), rest) = Heap.view queue
        elevation = elevations Map.! curr
        next = filter (\p -> Map.member p elevations && elevations Map.! p +1 >= elevation) $ getAround curr

getAround :: Pos -> [Pos]
getAround (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

part2 :: Input -> Output2
part2 (els, _, end) = dijkstra Set.empty els starts $ Heap.singleton (0, end)
    where starts = Map.keysSet $ Map.filter (==0) els
