module Days.Day15 where
import           Data.Char          (isDigit)
import           Data.List          (nub, sort)
import           Data.Maybe         (mapMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import qualified Program.RunDay     as R (runDay)
import qualified Program.TestDay    as T (testDay)
import           System.Clock       (TimeSpec)
import           Test.Hspec         (Spec)


runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 0 0

type Pos = (Int, Int)

type Input = [(Pos, Pos)]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map ((\[w,x,y,z] -> ((w,x),(y,z))) . map read . filter (not . null) . map (takeWhile (\x -> isDigit x || x == '-') . drop 2) . words) . lines


rowNum :: Int
rowNum = 2_000_000
-- rowNum = 10

part1 :: Input -> Output1
part1 ps = subtract bs $ scanLine minX maxX merged
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

getLineRanges :: Pos -> Pos -> [(Int, (Int, Int))]
getLineRanges p@(px, py) b = yLows ++ yHighs
    where
        maxDist = manhattenDist p b
        yLows  = dropWhile ((<0) . fst)         $ zip [(py-maxDist)..py-1] [(max 0 (px - xDist), min maxCoord (px + xDist)) | xDist <- [0..maxDist]]
        yHighs = takeWhile ((<=maxCoord) . fst) $ zip [py..(py+maxDist)] [(max 0 (px - xDist), min maxCoord (px + xDist)) | xDist <- [maxDist, maxDist-1 .. 0]]

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


maxCoord :: Int
maxCoord = 4_000_000
-- maxCoord = 20

part2 :: Input -> Output2
part2 = searchLines . Set.fromList . map (uncurry getLineRanges)

searchLines :: Set [(Int, (Int, Int))] -> Int
searchLines = go 0
    where
        go :: Int -> Set [(Int, (Int, Int))] -> Int
        go y ranges
            -- | y > maxCoord = error "Didn't find answer"
            | length rs > 1 = 4_000_000 * (1 + snd (head rs)) + y
            | otherwise = go (y+1) newSet
            where
                (inRange, outRange) = Set.spanAntitone ((==y) . fst . head) ranges
                newSet = Set.union outRange $ Set.filter (not . null) $ Set.map tail inRange
                rs = mergeRanges $ map (snd . head) $ Set.toList inRange
