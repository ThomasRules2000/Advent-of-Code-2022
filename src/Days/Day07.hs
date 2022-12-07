module Days.Day07 where
import           Data.List       (isSuffixOf)
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 95437 24933642

data Command = Cd Path | Ls (Set File)
    deriving (Eq, Ord, Show)

data Path = In String | Out | Root
    deriving (Eq, Ord, Show)

data File = Directory String | File String Int
    deriving (Eq, Ord, Show)

type Input = [Command]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map parseCommand . tail . splitOn "$ "

parseCommand :: String -> Command
parseCommand c = case take 2 c of
    "cd" -> Cd $ case init $ drop 3 c of
        "/"  -> Root
        ".." -> Out
        p    -> In p
    "ls" -> Ls $ Set.fromList $ map (parseFile . listToTuple . words) $ tail $ lines c
    where
        parseFile (size, name) = case size of
            "dir" -> Directory name
            _     -> File name $ read size

part1 :: Input -> Output1
part1 cs = sum $ Map.filter (<=100000) $ Map.mapWithKey (\k _ -> getSize k struct) struct
    where struct = getStructure cs

getStructure :: [Command] -> Map [String] (Set File)
getStructure = getStructure' []
    where
        getStructure' :: [String] -> [Command] -> Map [String] (Set File)
        getStructure' currPath ((Cd p):cs) = case p of
            Root   -> getStructure' [] cs
            Out    -> getStructure' (tail currPath) cs
            In dir -> getStructure' (dir:currPath) cs
        getStructure' currPath ((Ls s):cs) = Map.insert currPath s $ getStructure' currPath cs
        getStructure' _ [] = Map.empty

getSize :: [String] -> Map [String] (Set File) -> Int
getSize dir = sum . map (\case {File _ n -> n; _ -> 0}) . concat . Map.elems . fmap Set.toList . Map.filterWithKey (\k _ -> dir `isSuffixOf` k)

part2 :: Input -> Output2
part2 cs = minimum $ Map.filter (>=required) sizes
    where
        struct = getStructure cs
        sizes = Map.mapWithKey (\k _ -> getSize k struct) struct
        required = (sizes Map.! []) - 40000000
