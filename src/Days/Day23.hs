module Days.Day23 where
import qualified Data.Map.Strict  as Map
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Tuple.Extra (snd3, thd3)
import qualified Program.RunDay   as R (runDay)
import qualified Program.TestDay  as T (testDay)
import           System.Clock     (TimeSpec)
import           Test.Hspec       (Spec)
import qualified Util.Map         as Map
import qualified Util.Set         as Set

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 110 20

type Pos = (Int, Int)

data Direction = North | South | West | East
    deriving (Eq, Ord, Show)

type Input = Set Pos

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = Map.keysSet . Map.filter id . Map.fromGrid . map (map (=='#')) . lines

part1 :: Input -> Output1
part1 = getEmpty . snd3 . (!!10) . iterate doRound . (cycle [North, South, West, East], , False)

proposeMove :: [Direction] -> Set Pos -> Pos -> Maybe Pos
proposeMove dirs ps p@(x,y)
    | Set.disjoint ps $ getAround p = Nothing
    | otherwise = checkDirs $ take 4 dirs
    where
        checkDirs :: [Direction] -> Maybe Pos
        checkDirs [] = Nothing
        checkDirs (d:ds) = case d of
            North | all (`Set.notMember` ps) [(x-1, y+y0) | y0 <- [-1..1]] -> Just (x-1, y)
                  | otherwise -> checkDirs ds
            South | all (`Set.notMember` ps) [(x+1, y+y0) | y0 <- [-1..1]] -> Just (x+1, y)
                  | otherwise -> checkDirs ds
            East  | all (`Set.notMember` ps) [(x+x0, y+1) | x0 <- [-1..1]] -> Just (x, y+1)
                  | otherwise -> checkDirs ds
            West  | all (`Set.notMember` ps) [(x+x0, y-1) | x0 <- [-1..1]] -> Just (x, y-1)
                  | otherwise -> checkDirs ds

getAround :: Pos -> Set Pos
getAround (x1, y1) = Set.fromAscList [(x+x1, y+y1) | x <- [-1..1], y <- [-1..1], x/=0 || y /= 0]

doRound :: ([Direction], Set Pos, Bool) -> ([Direction], Set Pos, Bool)
doRound (dirs, ps, _) = (tail dirs, newSet, Set.null uniques)
    where
        proposals = Map.fromSet (proposeMove dirs ps) ps
        uniques = Set.catMaybes $ Map.keysSet $ Map.filter (==1) $ Map.unionsWith (+) $ map (`Map.singleton` (1::Int)) (Map.elems proposals)
        newSet = Set.union uniques $ Map.keysSet $ Map.filter (\case Just x -> x `Set.notMember` uniques; Nothing -> True) proposals

getEmpty :: Set Pos -> Int
getEmpty ps = ((maxX-minX + 1) * (maxY-minY + 1)) - Set.size ps
    where
        minX = Set.findMin $ Set.map fst ps
        maxX = Set.findMax $ Set.map fst ps
        minY = Set.findMin $ Set.map snd ps
        maxY = Set.findMax $ Set.map snd ps

part2 :: Input -> Output2
part2 = length . takeWhile (not . thd3) . iterate doRound . (cycle [North, South, West, East], , False)
