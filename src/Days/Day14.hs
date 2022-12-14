module Days.Day14 where
import           Data.List.Split (splitOn)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 24 93

type Pos = (Int, Int)

type Input = Set Pos

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = Set.unions . map (getRocks . map (listToTuple . map read . splitOn ",") . splitOn " -> ") . lines

getRocks :: [Pos] -> Set Pos
getRocks ((x1,y1):p2@(x2,y2):rest) = Set.union (Set.fromList newLine) $ getRocks (p2:rest)
    where
        newLine
            | x1 == x2 = [(x1,y) | y <- [y1, (y1+signum (y2-y1))..y2]]
            | otherwise = [(x,y1) | x <- [x1, (x1+signum (x2-x1))..x2]]
getRocks _ = Set.empty

part1 :: Input -> Output1
part1 = solve False

dropSand :: Bool -> Int -> Set Pos -> Int
dropSand isFloor maxY rocks = case moveSand isFloor maxY rocks (500, 0) of
    Nothing -> 0
    Just p | isFloor && p == (500,0) -> 1
           | otherwise -> 1 + dropSand isFloor maxY (Set.insert p rocks)

moveSand :: Bool -> Int -> Set Pos -> Pos -> Maybe Pos
moveSand isFloor maxY rocks (x, y)
    | y > maxY = if isFloor then Just (x,y) else Nothing -- Falling into the abyss/Settled on the floor
    | (x,y+1) `Set.notMember` rocks = moveSand isFloor maxY rocks (x, y+1) -- Move down
    | (x-1, y+1) `Set.notMember` rocks = moveSand isFloor maxY rocks (x-1, y+1) -- Move down left
    | (x+1, y+1) `Set.notMember` rocks = moveSand isFloor maxY rocks (x+1, y+1) -- Move down right
    | otherwise = Just (x,y) -- Settled on a rock or sand pile

solve :: Bool -> Set Pos -> Int
solve isFloor rocks = dropSand isFloor maxY rocks
    where maxY = Set.findMax $ Set.map snd rocks

part2 :: Input -> Output2
part2 = solve True
