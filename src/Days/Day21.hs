module Days.Day21 where
import           Data.Char                        (isDigit)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import qualified Program.RunDay                   as R (runDay)
import qualified Program.TestDay                  as T (testDay)
import           System.Clock                     (TimeSpec)
import           Test.Hspec                       (Spec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 152 301

data ASTNode = Node String String Op | Leaf Int
    deriving (Eq, Ord, Show)

data Op = Add | Sub | Mul | Div
    deriving (Eq, Ord, Show)

type Input = Map String ASTNode

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = Map.fromList . map parseLine . lines

parseLine :: String -> (String, ASTNode)
parseLine s
    | isDigit $ head (ws !! 1) = (nodeName, Leaf $ read $ ws !! 1)
    | otherwise = (nodeName, Node (ws!!1) (ws!!3) op)
    where
        ws = words s
        nodeName = init $ head ws
        op = case ws !! 2 of
            "+" -> Add
            "-" -> Sub
            "*" -> Mul
            "/" -> Div
            _   -> error "Invalid operation"

part1 :: Input -> Output1
part1 = getNum "root"

getNum :: String -> Map String ASTNode -> Int
getNum s m = case m Map.! s of
    Leaf x        -> x
    Node n1 n2 op -> case op of
        Add -> v1 + v2
        Sub -> v1 - v2
        Mul -> v1 * v2
        Div -> v1 `div` v2
        where
            v1 = getNum n1 m
            v2 = getNum n2 m

part2 :: Input -> Output2
part2 = flip getHumanNum 0 . (\(Unknown n1 n2 _) -> Unknown n1 n2 Sub) . getHumanTree "root"

data HumanTree = Unknown HumanTree HumanTree Op | Known Int | Human
    deriving (Eq, Ord, Show)

getHumanTree :: String -> Map String ASTNode -> HumanTree
getHumanTree s m
    | s == "humn" = Human
    | otherwise = case m Map.! s of
        Leaf x -> Known x
        Node n1 n2 op -> case (v1, v2) of
            (Known x1, Known x2) -> case op of
                Add -> Known $ x1 + x2
                Sub -> Known $ x1 - x2
                Mul -> Known $ x1 * x2
                Div -> Known $ x1 `div` x2
            _ -> Unknown v1 v2 op
            where
                v1 = getHumanTree n1 m
                v2 = getHumanTree n2 m

getHumanNum :: HumanTree -> Int -> Int
getHumanNum Human n = n
getHumanNum (Unknown n1 n2 op) n = case (n1, n2) of
    (Known v1, _) -> getHumanNum n2 $ case op of
        Add -> n - v1
        Sub -> v1 - n
        Mul -> n `div` v1
        Div -> v1 `div` n
    (_, Known v2) -> getHumanNum n1 $ case op of
        Add -> n - v2
        Sub -> n + v2
        Mul -> n `div` v2
        Div -> n * v2
    _ -> error "Invalid Tree"
getHumanNum (Known _) _ = error "Tried to get num of known value"
