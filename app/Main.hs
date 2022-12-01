module Main where
import           Control.Monad      (unless, when)
import           Data.List          (intercalate)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromMaybe, mapMaybe)
import           Data.Time.Calendar (toGregorian)
import           Data.Time.Clock    (utctDay, getCurrentTime)
import           System.Environment (getArgs)
import           Text.Printf        (printf)
import           Text.Read          (readMaybe)

{- ORMOLU_DISABLE -}
import qualified Days.Day01         as Day01 (runDay)
import qualified Days.Day02         as Day02 (runDay)
import qualified Days.Day03         as Day03 (runDay)
import qualified Days.Day04         as Day04 (runDay)
import qualified Days.Day05         as Day05 (runDay)
import qualified Days.Day06         as Day06 (runDay)
import qualified Days.Day07         as Day07 (runDay)
import qualified Days.Day08         as Day08 (runDay)
import qualified Days.Day09         as Day09 (runDay)
import qualified Days.Day10         as Day10 (runDay)
import qualified Days.Day11         as Day11 (runDay)
import qualified Days.Day12         as Day12 (runDay)
import qualified Days.Day13         as Day13 (runDay)
import qualified Days.Day14         as Day14 (runDay)
import qualified Days.Day15         as Day15 (runDay)
import qualified Days.Day16         as Day16 (runDay)
import qualified Days.Day17         as Day17 (runDay)
import qualified Days.Day18         as Day18 (runDay)
import qualified Days.Day19         as Day19 (runDay)
import qualified Days.Day20         as Day20 (runDay)
import qualified Days.Day21         as Day21 (runDay)
import qualified Days.Day22         as Day22 (runDay)
import qualified Days.Day23         as Day23 (runDay)
import qualified Days.Day24         as Day24 (runDay)
import qualified Days.Day25         as Day25 (runDay)
{- ORMOLU_ENABLE -}

days :: [(String -> IO (Maybe Integer, Maybe Integer), String)]
days = [
    (Day01.runDay, "input/Day01.txt"),
    (Day02.runDay, "input/Day02.txt"),
    (Day03.runDay, "input/Day03.txt"),
    (Day04.runDay, "input/Day04.txt"),
    (Day05.runDay, "input/Day05.txt"),
    (Day06.runDay, "input/Day06.txt"),
    (Day07.runDay, "input/Day07.txt"),
    (Day08.runDay, "input/Day08.txt"),
    (Day09.runDay, "input/Day09.txt"),
    (Day10.runDay, "input/Day10.txt"),
    (Day11.runDay, "input/Day11.txt"),
    (Day12.runDay, "input/Day12.txt"),
    (Day13.runDay, "input/Day13.txt"),
    (Day14.runDay, "input/Day14.txt"),
    (Day15.runDay, "input/Day15.txt"),
    (Day16.runDay, "input/Day16.txt"),
    (Day17.runDay, "input/Day17.txt"),
    (Day18.runDay, "input/Day18.txt"),
    (Day19.runDay, "input/Day19.txt"),
    (Day20.runDay, "input/Day20.txt"),
    (Day21.runDay, "input/Day21.txt"),
    (Day22.runDay, "input/Day22.txt"),
    (Day23.runDay, "input/Day23.txt"),
    (Day24.runDay, "input/Day24.txt"),
    (Day25.runDay, "input/Day25.txt")
    ]

bigDays :: [(String -> IO (Maybe Integer, Maybe Integer), String)]
bigDays = [
    (Day01.runDay, "input/Day01_big.txt"),
    (Day02.runDay, "input/Day02_big.txt"),
    (Day03.runDay, "input/Day03_big.txt"),
    (Day04.runDay, "input/Day04_big.txt"),
    (Day05.runDay, "input/Day05_big.txt"),
    (Day06.runDay, "input/Day06_big.txt"),
    (Day07.runDay, "input/Day07_big.txt"),
    (Day08.runDay, "input/Day08_big.txt"),
    (Day09.runDay, "input/Day09_big.txt"),
    (Day10.runDay, "input/Day10_big.txt"),
    (Day11.runDay, "input/Day11_big.txt"),
    (Day12.runDay, "input/Day12_big.txt"),
    (Day13.runDay, "input/Day13_big.txt"),
    (Day14.runDay, "input/Day14_big.txt"),
    (Day15.runDay, "input/Day15_big.txt"),
    (Day16.runDay, "input/Day16_big.txt"),
    (Day17.runDay, "input/Day17_big.txt"),
    (Day18.runDay, "input/Day18_big.txt"),
    (Day19.runDay, "input/Day19_big.txt"),
    (Day20.runDay, "input/Day20_big.txt"),
    (Day21.runDay, "input/Day21_big.txt"),
    (Day22.runDay, "input/Day22_big.txt"),
    (Day23.runDay, "input/Day23_big.txt"),
    (Day24.runDay, "input/Day24_big.txt"),
    (Day25.runDay, "input/Day25_big.txt")
    ]

testDays :: [(String -> IO (Maybe Integer, Maybe Integer), String)]
testDays = [
    (Day01.runDay, "input/Day01_test.txt"),
    (Day02.runDay, "input/Day02_test.txt"),
    (Day03.runDay, "input/Day03_test.txt"),
    (Day04.runDay, "input/Day04_test.txt"),
    (Day05.runDay, "input/Day05_test.txt"),
    (Day06.runDay, "input/Day06_test.txt"),
    (Day07.runDay, "input/Day07_test.txt"),
    (Day08.runDay, "input/Day08_test.txt"),
    (Day09.runDay, "input/Day09_test.txt"),
    (Day10.runDay, "input/Day10_test.txt"),
    (Day11.runDay, "input/Day11_test.txt"),
    (Day12.runDay, "input/Day12_test.txt"),
    (Day13.runDay, "input/Day13_test.txt"),
    (Day14.runDay, "input/Day14_test.txt"),
    (Day15.runDay, "input/Day15_test.txt"),
    (Day16.runDay, "input/Day16_test.txt"),
    (Day17.runDay, "input/Day17_test.txt"),
    (Day18.runDay, "input/Day18_test.txt"),
    (Day19.runDay, "input/Day19_test.txt"),
    (Day20.runDay, "input/Day20_test.txt"),
    (Day21.runDay, "input/Day21_test.txt"),
    (Day22.runDay, "input/Day22_test.txt"),
    (Day23.runDay, "input/Day23_test.txt"),
    (Day24.runDay, "input/Day24_test.txt"),
    (Day25.runDay, "input/Day25_test.txt")
    ]

main :: IO ()
main = do
    args <- getArgs
    toRun <- case (if "-a" `elem` args then [1..25] else mapMaybe readMaybe args) of
                    [] -> (toGregorian . utctDay) <$> getCurrentTime >>= \case
                        (_, 12, d) | d <= 25 -> return [d]
                        _ -> error "Usage: stack run n"
                    xs -> return xs
    let ds | "--test" `elem` args = testDays
           | "--big"  `elem` args = bigDays
           | otherwise            = days
    times <- Map.fromList . zip toRun <$> mapM (\arg -> uncurry (performDay arg) $ fromMaybe (error $ printf "Day %d not found" arg) $ lookup arg (zip [1..] ds)) toRun
    printSummary times

performDay :: Int -> (String -> IO (Maybe Integer, Maybe Integer)) -> String -> IO (Maybe Integer, Maybe Integer)
performDay day func file = do
    putStrLn $ printf "===== DAY %d =====" day
    func file

printSummary :: Map Int (Maybe Integer, Maybe Integer) -> IO ()
printSummary results = do
    putStrLn "====== SUMMARY ======"
    let part1s = Map.mapKeys ((++".1") . show) $ fst <$> results
        part2s = Map.mapKeys ((++".2") . show) $ snd <$> results
        parts  = Map.toList $ part1s <> part2s

        fails = [p | (p, Nothing) <- parts]
        fasts = [(p, t) | (p, Just t) <- parts, t <  10^12]
        slows = [(p, t) | (p, Just t) <- parts, t >= 10^12]

        greatSuccess = null fails && null slows
        nFails = if null fails then "No" else show $ length fails
        nFasts = if null fasts then "No" else show $ length fasts
        nSlows = if null slows then "No" else show $ length slows

    when greatSuccess $ putStr "All "
    putStr $ printf "%s parts completed under 1 second" nFasts
    unless greatSuccess $ do
        putStrLn "\n Of the remainder:"
        unless (null fails) $ do
            putStrLn $ printf "  %s parts failed" nFails
            putStrLn $ "    " ++ intercalate ", " fails
        unless (null slows) $ do
            putStrLn $ printf "  %s parts took longer than 1s" nSlows
            putStrLn $ "    " ++ intercalate ", " (map (uncurry (printf "%s (%.2fs)")) $ fmap ((/10^12) . fromIntegral :: Integer -> Double) <$> slows)
