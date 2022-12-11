module Days.Day11 where
import           Data.Bifunctor     (bimap)
import           Data.Char          (isDigit)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List          (sort)
import           Data.List.Split    (splitOn)
import           Data.Sequence      (Seq)
import qualified Data.Sequence      as Seq
import qualified Program.RunDay     as R (runDay)
import qualified Program.TestDay    as T (testDay)
import           System.Clock       (TimeSpec)
import           Test.Hspec         (Spec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 62491 2713310158

data Monkey = Monkey {
    items       :: Seq Integer,
    op          :: Integer -> Integer,
    test        :: Integer -> Bool,
    trueThrow   :: Int,
    falseThrow  :: Int,
    inspections :: Integer
}

type Input = (IntMap Monkey, Integer)

type Output1 = Integer
type Output2 = Integer

parser :: String -> Input
parser = bimap (IntMap.fromDistinctAscList . zip [0..]) (foldr lcm 1) . unzip . map parseMonkey . splitOn "\n\n"

parseMonkey :: String -> (Monkey, Integer)
parseMonkey s = (Monkey {..}, testN)
    where
        [iWords, opWords, testWords, trueWords, falseWords] = map words $ tail $ lines s
        items = Seq.fromList $ map (read . filter isDigit) $ drop 2 iWords
        op x = case opWords !! 4 of
            "*" -> case last opWords of
                "old" -> x * x
                _     -> x * n
            "+" -> x + n
            where n = read $ last opWords
        test x = x `mod` testN == 0
        testN = read $ last testWords
        trueThrow = read $ last trueWords
        falseThrow = read $ last falseWords
        inspections = 0

part1 :: Input -> Output1
part1 = solve (`div` 3) 20 . fst

solve :: (Integer -> Integer) -> Int -> IntMap Monkey -> Integer
solve f n monkeyMap = product
                    $ take 2
                    $ reverse
                    $ sort
                    $ map inspections
                    $ IntMap.elems
                    $ (!!n)
                    $ iterate (\mm -> foldl (processMonkey f) mm monkeys) monkeyMap
                    where monkeys = IntMap.keys monkeyMap

processMonkey :: (Integer -> Integer) -> IntMap Monkey -> Int -> IntMap Monkey
processMonkey f im n = IntMap.insert n m{items=Seq.empty, inspections=inspections m + fromIntegral (Seq.length $ items m)} $ foldl processItem im $ items m
    where
        m = im IntMap.! n
        processItem :: IntMap Monkey -> Integer -> IntMap Monkey
        processItem im' w = IntMap.insert newMonkeyNum newMonkey{items=items newMonkey Seq.|> newWorry} im'
            where
                newWorry = f $ op m w
                newMonkeyNum = if test m newWorry then trueThrow m else falseThrow m
                newMonkey = im' IntMap.! newMonkeyNum

part2 :: Input -> Output2
part2 (im, d) = solve (`mod` d) 10000 im
