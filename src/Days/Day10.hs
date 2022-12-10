module Days.Day10 where
import           Data.Composition ((.:))
import           Data.List.Split  (chunksOf)
import           Data.Matrix      (Matrix)
import qualified Data.Matrix      as Matrix
import qualified Program.RunDay   as R (runDay)
import qualified Program.TestDay  as T (testDay)
import           System.Clock     (TimeSpec)
import           Test.Hspec       (Spec)
import qualified Util.Picture     as Picture
import           Util.Picture     (Picture (..))
import           Util.Util        (takeEveryN)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 13140
    (Picture.fromHashDot $ unlines ["##..##..##..##..##..##..##..##..##..##..",
                                    "###...###...###...###...###...###...###.",
                                    "####....####....####....####....####....",
                                    "#####.....#####.....#####.....#####.....",
                                    "######......######......######......####",
                                    "#######.......#######.......#######....."])

data Instruction = NoOp | AddX Int
    deriving (Eq, Ord, Show)

type Input = [Instruction]

type Output1 = Int
type Output2 = Picture

parser :: String -> Input
parser = map (getInstruction . words) . lines
    where
        getInstruction :: [String] -> Instruction
        getInstruction ["noop"]    = NoOp
        getInstruction ["addx", n] = AddX $ read n
        getInstruction i           = error $ "Invalid Instruction: " ++ show i

part1 :: Input -> Output1
part1 = sum 
      . zipWith (*) [20,60..] 
      . takeEveryN 40 
      . drop 19 
      . getSeq 1

getSeq :: Int -> [Instruction] -> [Int]
getSeq x []            = []
getSeq x (NoOp:is)     = x : getSeq x is
getSeq x ((AddX n):is) = x : x : getSeq (x+n) is

part2 :: Input -> Output2
part2 = Picture
      . Matrix.fromLists
      . chunksOf 40
      . zipWith ((<=1) . abs .: (-)) (cycle [0..39])
      . getSeq 1
