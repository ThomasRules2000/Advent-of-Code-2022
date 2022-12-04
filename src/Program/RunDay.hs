{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Program.RunDay where
import           Control.Exception      (SomeException, evaluate, try)
import           Control.Monad.Extra    (unlessM)
import qualified Data.ByteString.Char8  as BS
import           Data.Either.Extra      (eitherToMaybe)
import           Data.Functor           (($>))
import           Formatting             (formatToString)
import           Formatting.Clock       (timeSpecs)
import           Network.HTTP.Simple
import           System.Clock           (Clock (Monotonic), TimeSpec, getTime)
import           System.CPUTime         (getCPUTime)
import           System.Directory.Extra (doesFileExist)
import           System.Exit            (exitFailure)
import           Text.Printf            (printf)
import           Text.Read              (readMaybe)

runDay :: (Show out1, Show out2) => (String -> inp) -> (inp -> out1) -> (inp -> out2) -> String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay parser part1 part2 s = do
    unlessM (doesFileExist s) $ downloadFile s
    parserStart <- getTime Monotonic
    file <- try $ readFile s >>= (evaluate . parser)
    parserEnd <- getTime Monotonic
    case file of
        Left (e :: SomeException) -> putStrLn "Unable to parse input!" >> print e $> (Nothing, Nothing, Nothing)
        Right input -> do
            let parserTime = parserEnd - parserStart
            putStrLn $ "Parser (" ++ formatToString timeSpecs parserStart parserEnd ++ ")"

            p1Start <- getTime Monotonic
            p1Res <- try $ evaluate $ part1 input
            p1End <- getTime Monotonic

            let p1Time = p1End - p1Start
            putStrLn $ "Part 1 (" ++ formatToString timeSpecs p1Start p1End ++ "):"
            putStrLn $ either (\(e :: SomeException) -> "Unable to run Part 1!\n" ++ show e) show p1Res

            p2Start <- getTime Monotonic
            p2Res <- try (evaluate (part2 input))
            p2End <- getTime Monotonic

            let p2Time = p2End - p2Start
            putStrLn $ printf "Part 2 (" ++ formatToString timeSpecs p2Start p2End ++ "):"
            putStrLn $ either (\(e :: SomeException) -> "Unable to run Part 2!\n" ++ show e) show p2Res

            putStrLn $ "Total Time: " ++ formatToString timeSpecs 0 (parserTime + p1Time + p2Time)

            return (Just parserTime, eitherToMaybe p1Res $> p1Time, eitherToMaybe p2Res $> p2Time)

downloadFile :: String -> IO ()
downloadFile f = case readMaybe @Int $ take 2 $ drop 9 f of
    Nothing -> putStrLn ("Can't find file " ++ f ++ "!") >> exitFailure
    Just n -> do
        key <- BS.readFile "sessionkey.txt"
        req <- addRequestHeader "Cookie" ("session=" <> key) <$> parseRequest ("https://adventofcode.com/2022/day/" ++ show n ++ "/input")
        resp <- httpBS req
        case getResponseStatusCode resp of
            200 -> BS.writeFile f $ getResponseBody resp
            _ -> do
                putStrLn $ "Unable to download " <> f <> " from the server"
                putStrLn $ "Response Code: " <> show (getResponseStatusCode resp)
                putStrLn "Response: "
                BS.putStrLn $ getResponseBody resp
                exitFailure
