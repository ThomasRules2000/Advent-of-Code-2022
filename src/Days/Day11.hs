module Days.Day11 where
import           Control.Lens        (foldByOf, folded, makeLenses, view, (%~),
                                      (+~), (.~), (^.))
import           Control.Monad       (join, replicateM_)
import           Control.Monad.ST    (ST, runST)
import           Data.Bifunctor      (first)
import           Data.Char           (isDigit)
import           Data.Composition    ((.:))
import           Data.List           (sortOn)
import           Data.List.Split     (splitOn)
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as Seq
import           Data.Tuple.Extra    (dupe)
import qualified Data.Vector         as Vector
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector
import qualified Program.RunDay      as R (runDay)
import qualified Program.TestDay     as T (testDay)
import           System.Clock        (TimeSpec)
import           Test.Hspec          (Spec)

data Monkey = Monkey {
    _items       :: Seq Int,
    _op          :: Int -> Int,
    _test        :: Int,
    _trueThrow   :: Int,
    _falseThrow  :: Int,
    _inspections :: Int
}
makeLenses ''Monkey

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 10605 2713310158

type Input = [Monkey]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map parseMonkey . splitOn "\n\n"

parseMonkey :: String -> Monkey
parseMonkey s = Monkey {..}
    where
        [iWords, opWords, testWords, trueWords, falseWords] = map words $ tail $ lines s
        _items = Seq.fromList $ map (read . filter isDigit) $ drop 2 iWords
        _op = case opWords !! 4 of
            "*" -> case last opWords of
                "old" -> (^2)
                _     -> (*n)
            "+" -> (+n)
            _   -> error $ "Invalid Operation " ++ show opWords
            where n = read $ last opWords
        _test = read $ last testWords
        _trueThrow = read $ last trueWords
        _falseThrow = read $ last falseWords
        _inspections = 0

part1 :: Input -> Output1
part1 = solve 20 (`div` 3)

solve :: Int -> (Int -> Int) -> [Monkey] -> Int
solve n f ms = product $ take 2 $ sortOn negate $ map _inspections $ Vector.toList $ runST $ do
    v <- Vector.thaw $ Vector.fromList ms
    replicateM_ n $ mapM_ (processMonkey f v) monkeys
    Vector.freeze v
    where monkeys = [0..length ms - 1]

processMonkey :: (Int -> Int) -> MVector s Monkey -> Int -> ST s ()
processMonkey f v n = do
    m <- MVector.read v n
    let processItem w = MVector.modify v (items %~ (Seq.|> newWorry)) $
            if newWorry `mod` m ^. test == 0
                then m ^. trueThrow
                else m ^. falseThrow
            where newWorry = (f .: view op) m w

    mapM_ processItem $ m ^. items
    MVector.modify v ((items .~ Seq.empty) . join ((inspections +~) . (Seq.length . view items))) n

part2 :: Input -> Output2
part2 = uncurry (solve 10000) . first (flip mod . foldByOf (folded . test) lcm 1) . dupe
