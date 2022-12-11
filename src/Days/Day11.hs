module Days.Day11 where
import           Control.Monad       (replicateM_)
import           Control.Monad.ST    (ST, runST)
import           Data.Bifunctor      (bimap)
import           Data.Char           (isDigit)
import           Data.List           (sort)
import           Data.List.Split     (splitOn)
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as Seq
import           Data.Vector         (Vector)
import qualified Data.Vector         as Vector
import           Data.Vector.Mutable (MVector, STVector)
import qualified Data.Vector.Mutable as MVector
import qualified Program.RunDay      as R (runDay)
import qualified Program.TestDay     as T (testDay)
import           System.Clock        (TimeSpec)
import           Test.Hspec          (Spec)

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

type Input = ([Monkey], Integer)

type Output1 = Integer
type Output2 = Integer

parser :: String -> Input
parser = fmap (foldr lcm 1) . unzip . map parseMonkey . splitOn "\n\n"

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

solve :: (Integer -> Integer) -> Int -> [Monkey] -> Integer
solve f n ms = product $ take 2 $ reverse $ sort $ map inspections $ Vector.toList $ runST $ do
    v <- Vector.thaw $ Vector.fromList ms
    replicateM_ n $ mapM_ (processMonkey f v) monkeys
    Vector.freeze v
    where monkeys = [0..length ms - 1]

processMonkey :: (Integer -> Integer) -> MVector s Monkey -> Int -> ST s ()
processMonkey f v n = do
    m <- MVector.read v n

    let processItem w = do
            let newWorry = f $ op m w
            let newMonkeyNum = if test m newWorry then trueThrow m else falseThrow m
            newMonkey <- MVector.read v newMonkeyNum
            MVector.write v newMonkeyNum $ newMonkey{items=items newMonkey Seq.|> newWorry}

    mapM_ processItem $ items m
    MVector.write v n m{items=Seq.empty, inspections=inspections m + fromIntegral (Seq.length $ items m)}

part2 :: Input -> Output2
part2 (im, d) = solve (`mod` d) 10000 im
