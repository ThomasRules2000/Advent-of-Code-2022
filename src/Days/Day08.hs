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
import           Util.Util       (takeWhileIncl)

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
part1 m = length $ filter (visible m maxPos) $ Map.keys m
    where maxPos = Set.findMax $ Map.keysSet m

visible :: Map Pos Int -> Pos -> Pos -> Bool
visible m maxPos p = any (all ((<tree) . (m Map.!))) $ getLines maxPos p
    where tree  = m Map.! p

getLines :: Pos -> Pos -> [[Pos]]
getLines (xMax, yMax) (x0, y0) = [north, south, east, west]
    where
        north = [(x, y0) | x <- [x0-1, x0-2..0]]
        south = [(x, y0) | x <- [x0+1..xMax]]
        east  = [(x0, y) | y <- [y0-1, y0-2..0]]
        west  = [(x0, y) | y <- [y0+1..yMax]]

part2 :: Input -> Output2
part2 m = maximum $ map (scenicScore m maxPos) $ [(x,y) | x <- [1..xMax-1], y <- [1..yMax-1]]
    where maxPos@(xMax, yMax) = Set.findMax $ Map.keysSet m

scenicScore :: Map Pos Int -> Pos -> Pos -> Int
scenicScore m maxPos p = product $ map (length . takeWhileIncl (<tree) . map (m Map.!)) $ getLines maxPos p
    where tree = m Map.! p
