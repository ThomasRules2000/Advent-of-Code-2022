module Days.Day14 where
import           Control.Monad.Extra              (ifM)
import           Control.Monad.Trans.State.Strict (State, evalState, gets,
                                                   modify)
import           Data.Bifunctor                   (first, second)
import           Data.Composition                 ((.:))
import           Data.Functor                     (($>))
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
getRocks ps = Set.unions $ zipWith (Set.fromList .: newLine) ps $ tail ps
    where newLine (x1, y1) (x2, y2)
            | x1 == x2  = [(x1,y) | y <- [y1, (y1+signum (y2-y1))..y2]]
            | otherwise = [(x,y1) | x <- [x1, (x1+signum (x2-x1))..x2]]

part1 :: Input -> Output1
part1 = solve False

numReachable :: Bool -> Int -> Pos -> State (Set Pos, Bool) Int
numReachable isFloor maxY curr@(x,y)
    -- If we go below maxY, for Part 1 we are done as this is the abyss, for Part 2 this is the floor so put the sand there
    | y > maxY = modify (if isFloor then first $ Set.insert curr else second $ const True) $> fromEnum isFloor
    -- Otherwise, if we aren't done and we haven't already checked this square, consider the square
    | otherwise = ifM (gets $ uncurry (flip (||)) . first (curr `Set.member`)) (return 0) $ do
        nextPs <- gets $ (\rocks -> filter (`Set.notMember` rocks) [(x, y+1), (x-1, y+1), (x+1, y+1)]) . fst
        -- First check the up to 3 squares the sand could move in to which aren't rocks
        n <- sum <$> mapM (numReachable isFloor maxY) nextPs
        -- If there is now sand in the places where rocks weren't, put sand in the current square
        ifM ((`any` nextPs) <$> gets (flip Set.notMember . fst)) (return n) $ modify (first $ Set.insert curr) $> (n + 1)

solve :: Bool -> Set Pos -> Int
solve isFloor rocks = evalState (numReachable isFloor maxY (500,0)) (rocks, False)
    where maxY = Set.findMax $ Set.map snd rocks

part2 :: Input -> Output2
part2 = solve True
