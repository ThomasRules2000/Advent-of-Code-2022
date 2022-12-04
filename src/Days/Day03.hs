module Days.Day03 where
import           Data.Char        (ord)
import           Data.List.Split  (chunksOf)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Tuple.Extra (both)
import qualified Program.RunDay   as R (runDay)
import           System.Clock     (TimeSpec)
import qualified Util.Set         as Set

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

type Input = [(Set Char, Set Char)]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (both Set.fromList . \s -> splitAt (length s `div` 2) s) . lines

part1 :: Input -> Output1
part1 = sum . map (priority . Set.findMin . uncurry Set.intersection)

priority :: Char -> Int
priority c
    | 'a' <= c && c <= 'z' = 1 + ord c - ord 'a'
    | 'A' <= c && c <= 'Z' = 27 + ord c - ord 'A'
    | otherwise = undefined

part2 :: Input -> Output2
part2 = sum . map (priority . Set.findMin . Set.intersections) . chunksOf 3 . map (uncurry Set.union)
