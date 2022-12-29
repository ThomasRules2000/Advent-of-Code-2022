module Days.Day15 where
import           Data.Char                        (isDigit)
import           Data.List                        (nub, sort)
import           Data.Maybe                       (mapMaybe)
import           Data.Tuple.Extra                 (both)
import qualified Program.RunDay                   as R (runDay)
import qualified Program.TestDay                  as T (testDay)
import           System.Clock                     (TimeSpec)
import           Test.Hspec                       (Spec)


runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser (part1 2_000_000) (part2 4_000_000)

testDay :: String -> String -> Spec
testDay = T.testDay parser (part1 10) (part2 20) 26 56000011

type Pos = (Int, Int)

type Input = [(Pos, Pos)]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map ((\[w,x,y,z] -> ((w,x),(y,z))) . map read . filter (not . null) . map (takeWhile (\x -> isDigit x || x == '-') . drop 2) . words) . lines


part1 :: Int -> Input -> Output1
part1 rowNum ps = subtract bs $ scanLine minX maxX merged
    where
        bs = length $ nub $ map fst $ filter ((==rowNum) . snd) $ map snd ps
        merged = mergeRanges $ mapMaybe (uncurry $ getLineRange rowNum) ps
        minX = fst $ head merged
        maxX = snd $ last merged

manhattenDist :: Pos -> Pos -> Int
manhattenDist (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)

getLineRange :: Int -> Pos -> Pos -> Maybe (Int, Int)
getLineRange y p@(px,py) b = if xDist > 0 then Just (px - xDist, px + xDist) else Nothing
    where
        maxDist = manhattenDist p b
        yDist = abs $ py - y
        xDist = maxDist - yDist
        
mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges = go . sort
    where
        go :: [(Int, Int)] -> [(Int, Int)]
        go (x@(xMin, xMax):y@(yMin,yMax):rest)
            | xMax >= yMin - 1 = go $ (xMin, max xMax yMax):rest
            | otherwise = x : go (y:rest)
        go xs = xs

scanLine :: Int -> Int -> [(Int, Int)] -> Int
scanLine minX maxX = go minX
    where
        go :: Int -> [(Int, Int)] -> Int
        go _ [] = 0
        go currX rrs@((rxMin, rxMax):rs)
            | currX > maxX = 0
            | currX > rxMax = go currX rs
            | currX >= rxMin = (rxMax - currX + 1) + go (rxMax + 1) rs
            | otherwise = go (currX+1) rrs


part2 :: Int -> Input -> Output2
part2 maxCoord = (\(x,y) -> 4_000_000 * x + y) . uncurry searchDiags . fmap concat . unzip . map (uncurry $ getDiags maxCoord)

getDiags :: Int -> Pos -> Pos -> ((Pos, Int), [Pos])
getDiags maxCoord p@(px, py) b = ((p, dist-1), outer)
    where
        dist = 1 + manhattenDist p b
        outer = filter (uncurry (&&) . both (\x -> x >= 0 && x <= maxCoord)) $ concat [[(xMin+d, py+d), (xMax-d, py+d), (xMin+d, py-d), (xMax-d, py-d)] | d <- [0..dist]]
        xMin = px - dist
        xMax = px + dist

searchDiags :: [(Pos, Int)] -> [Pos] -> Pos
searchDiags sensors = head . filter (\c -> all (\(p, d) -> manhattenDist p c > d) sensors)
