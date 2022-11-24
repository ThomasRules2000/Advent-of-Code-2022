{-# LANGUAGE ScopedTypeVariables #-}
module Program.RunDay where
import           Control.Exception (SomeException, evaluate, try)
import           Data.Either.Extra (eitherToMaybe)
import           Data.Functor      (($>))
import           System.CPUTime    (getCPUTime)
import           Text.Printf       (printf)

runDay :: (Show out1, Show out2) => (String -> inp) -> (inp -> out1) -> (inp -> out2) -> String -> IO (Maybe Integer, Maybe Integer)
runDay parser part1 part2 s = do
    parserStart <- getCPUTime
    file <- try $ readFile s >>= (evaluate . parser)
    parserEnd <- getCPUTime
    case file of
        Left (e :: SomeException) -> putStrLn "Unable to parse input!" >> print e >> return (Nothing, Nothing)
        Right input -> do
            let parserTime = parserEnd - parserStart
            putStrLn $ "Parser " ++ timeString parserTime

            p1Start <- getCPUTime
            p1Res <- try $ evaluate $ part1 input
            p1End <- getCPUTime

            let p1Time = p1End - p1Start
            putStrLn $ "Part 1 " ++ timeString p1Time ++ ":"
            putStrLn $ either (\(e :: SomeException) -> "Unable to run Part 1!\n" ++ show e) show p1Res

            p2Start <- getCPUTime
            p2Res <- try (evaluate (part2 input))
            p2End <- getCPUTime

            let p2Time = p2End - p2Start
            putStrLn $ printf "Part 2 " ++ timeString p2Time ++ ":"
            putStrLn $ either (\(e :: SomeException) -> "Unable to run Part 2!\n" ++ show e) show p2Res

            return (eitherToMaybe p1Res $> p1Time, eitherToMaybe p2Res $> p2Time)


timeString :: Integer -> String
timeString t
    | t > 10^11 = printf "(%.2fs)"  (fromIntegral t / (10^12) :: Double)
    | t > 10^8  = printf "(%.2fms)" (fromIntegral t / (10^9)  :: Double)
    | t > 10^5  = printf "(%.2fμs)" (fromIntegral t / (10^6)  :: Double)
    | t > 100   = printf "(%.2fns)" (fromIntegral t / (10^3)  :: Double)
    | otherwise = printf "(%dps)" t
