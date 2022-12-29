module Days.Day20 where
import           Control.Monad               (replicateM_)
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.ST            (runST)
import           Data.List                   (sortOn)
import qualified Data.Vector.Unboxed         as Vector
import           Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MVector
import qualified Program.RunDay              as R (runDay)
import qualified Program.TestDay             as T (testDay)
import           System.Clock                (TimeSpec)
import           Test.Hspec                  (Spec)
import           Util.Util                   (takeEveryN)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 0 0

type Input = [Int]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map read . lines

part1 :: Input -> Output1
part1 = solve 1

mix :: (PrimMonad m) => MVector (PrimState m) (Int, Int) -> Int -> m ()
mix v l = mapM_ go [0..l-1]
    where
        go n = do
            (x, p) <- MVector.read v n
            let newP = (p + x) `mod` (l-1)
            let moveLeft i' (x', p')
                    | p == p' = MVector.write v i' (x', newP)
                    | p' >= newP && p' <= p = MVector.write v i' (x', p' + 1)
                    | otherwise = return () -- out of range
            let moveRight i' (x', p')
                    | p == p' = MVector.write v i' (x', newP)
                    | p' <= newP && p' >= p = MVector.write v i' (x', p' - 1)
                    | otherwise = return () -- out of range
            if | newP < p  -> MVector.imapM_ moveLeft v
               | newP > p  -> MVector.imapM_ moveRight v
               | otherwise -> return () -- Hasn't moved


solve :: Int -> [Int] -> Int
solve n xs = sum $ take 3 $ tail $ takeEveryN 1000 res
    where res = dropWhile (/=0) $ cycle $ map fst $ sortOn snd $ Vector.toList $ runST $ do
            v <- Vector.unsafeThaw $ Vector.fromList $ zip xs [0..]
            replicateM_ n (mix v $ length xs)
            Vector.unsafeFreeze v

part2 :: Input -> Output2
part2 = solve 10 . map (*811589153)
