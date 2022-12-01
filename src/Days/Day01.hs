module Days.Day01 where
import           Data.List       (sort)
import           Data.List.Split (splitOn)
import qualified Program.RunDay  as R (runDay)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = [[Int]]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (map read . lines) . splitOn "\n\n"

part1 :: Input -> Output1
part1 = maximum . map sum

part2 :: Input -> Output2
part2 = sum . take 3 . reverse . sort . map sum
