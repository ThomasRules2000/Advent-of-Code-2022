module Days.Day09 where
import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Integer, Maybe Integer)
runDay = R.runDay parser part1 part2

type Input = [Int]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = undefined

part1 :: Input -> Output1
part1 = undefined

part2 :: Input -> Output2
part2 = undefined
