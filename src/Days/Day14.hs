module Days.Day14 where
import           Control.Monad                    (when, unless)
import           Control.Monad.Trans.State.Strict (State, execState, gets,
                                                   modify)
import           Data.Bifunctor                   (first, second)
import           Data.List.Split                  (splitOn)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import qualified Program.RunDay                   as R (runDay)
import qualified Program.TestDay                  as T (testDay)
import           System.Clock                     (TimeSpec)
import           Test.Hspec                       (Spec)
import           Util.Util                        (listToTuple)

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

numReachable :: Bool -> Set Pos -> Int -> Pos -> State (Set Pos, Bool) ()
numReachable isFloor rocks maxY curr@(x,y)
    -- If we go below maxY, for Part 1 we are done as this is the abyss, for Part 2 this is the floor so put the sand there
    | y > maxY = modify $ if isFloor then first $ Set.insert curr else second $ const True
    -- Otherwise, if we aren't done and we haven't already checked this square, consider the square
    | otherwise = gets (uncurry (flip (||)) . first (curr `Set.member`)) >>= flip unless (do
        -- First check the up to 3 squares the sand could move in to which aren't rocks
        mapM_ (numReachable isFloor rocks maxY) nextPs
        -- Get the new places where there is sand
        sand <- gets fst
        -- If there is now sand in the places where rocks weren't, put sand in the current square
        when (all (`Set.member` sand) nextPs) $ modify $ first $ Set.insert curr)
        where nextPs = filter (`Set.notMember` rocks) [(x, y+1), (x-1, y+1), (x+1, y+1)]

solve :: Bool -> Set Pos -> Int
solve isFloor rocks = Set.size $ fst $ execState (numReachable isFloor rocks maxY (500,0)) (Set.empty, False)
    where maxY = Set.findMax $ Set.map snd rocks

part2 :: Input -> Output2
part2 = solve True
