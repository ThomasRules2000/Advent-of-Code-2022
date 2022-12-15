module Days.Day15 where
import           Data.Char       (isDigit)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)


runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 0 0

type Pos = (Int, Int)

type Input = [(Pos, Pos)]

type Output1 = Int
type Output2 = Int

-- 2,3,8,9
parser :: String -> Input
parser = map ((\[w,x,y,z] -> ((w,x),(y,z))) . map read . filter (not . null) . map (takeWhile (\x -> isDigit x || x == '-') . drop 2) . words) . lines

part1 :: Input -> Output1
part1 ps = Set.size $ (Set.\\ bs) $ Set.unions $ map (uncurry getLinePoints) ps
    where bs = Set.fromList $ map fst $ filter ((==2000000) . snd) $ map snd ps

manhattenDist :: Pos -> Pos -> Int
manhattenDist (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)

getLinePoints :: Pos -> Pos -> Set Int
getLinePoints p@(px,py) b = if xDist > 0 then Set.fromList [px - xDist .. px + xDist] else Set.empty
    where
        maxDist = manhattenDist p b
        yDist = abs $ py - 2000000
        xDist = maxDist - yDist

part2 :: Input -> Output2
part2 = undefined
