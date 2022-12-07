module Days.Day07 where
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import Data.List.Split (splitOn)
import Util.Util (listToTuple)
import Debug.Trace (traceShowId)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 0 0

data Command = Cd Path | Ls [File]
    deriving (Eq, Ord, Show)

data Path = In String | Out | Root
    deriving (Eq, Ord, Show)

data File = Dir String | File String Int
    deriving (Eq, Ord, Show)

type Input = [Command]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = traceShowId . map parseCommand . splitOn "\n$ "

parseCommand :: String -> Command
parseCommand c = case take 2 c of
    "cd" -> Cd $ case drop 3 c of
        "/" -> Root
        ".." -> Out
        p -> In p
    "ls" -> Ls $ map (parseFile . listToTuple . words) $ tail $ lines c
    where
        parseFile (size, name) = case size of
            "dir" -> Dir name
            _ -> File name $ read size

part1 :: Input -> Output1
part1 = undefined

part2 :: Input -> Output2
part2 = undefined
