module Days.Day09 where
import           Data.Bifunctor  (bimap)
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
testDay = T.testDay parser part1 part2 13 1

data Move = U | D | L | R
    deriving (Eq, Ord, Show, Read)

type Pos = (Int, Int)

type Input = [(Move, Int)]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (bimap read read . listToTuple . words) . lines

part1 :: Input -> Output1
part1 = getAns 2

getAns :: Int -> [(Move, Int)] -> Int
getAns n = Set.size . snd . foldl (\(ps, s) m -> Set.union s <$> doMove ps m) (replicate n (0,0), Set.empty)

getAround :: Pos -> [Pos]
getAround (x0, y0) = [(x+x0,y+y0) | x <- [-1..1], y <- [-1..1]]

doMove :: [Pos] -> (Move, Int) -> ([Pos], Set Pos)
doMove ps (m, n) = (last newPos, Set.fromList $ map last newPos)
    where newPos = take (n+1) $ iterate (moveSingle m) ps

moveSingle :: Move -> [Pos] -> [Pos]
moveSingle _ []            = []
moveSingle m ((hx, hy):ps) = moveKnot newHead ps
    where newHead = case m of
                        U -> (hx, hy-1)
                        D -> (hx, hy+1)
                        L -> (hx-1, hy)
                        R -> (hx+1, hy)

moveKnot :: Pos -> [Pos] -> [Pos]
moveKnot prev [] = [prev]
moveKnot prev@(px, py) (next@(nx, ny):rest) = prev : moveKnot newNext rest
    where newNext
              | next `elem` getAround prev = next
              | px == nx || py == ny = head $ filter (`elem` getAround prev) [(nx+1, ny), (nx-1, ny), (nx, ny+1), (nx, ny-1)]
              | otherwise            = head $ filter (`elem` getAround prev) [(x+nx, y+ny) | x <- [-1,1], y <- [-1,1]]

part2 :: Input -> Output2
part2 = getAns 10
