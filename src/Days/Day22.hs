module Days.Day22 where
import           Data.Bifunctor  (bimap, first)
import           Data.Char       (isDigit)
import           Data.List       (foldl')
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.Cycle      (Cycle, next, prev)
import qualified Util.Map        as Map
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 (part2 cubeEdgeMap 50)

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 (part2 testCubeEdgeMap 4) 6032 5031

type Pos = (Int, Int)

data Move = Forward Int | TurnLeft | TurnRight
    deriving (Eq, Ord, Show)

data Facing = East | South | West | North
    deriving (Eq, Ord, Show, Enum, Bounded, Cycle)

type Input = (Map Pos Bool, [Move])

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = bimap parseGrid parseMoves . listToTuple . splitOn "\n\n"

parseGrid :: String -> Map Pos Bool
parseGrid = Map.catMaybes . Map.fromGrid . map (map getSquare) . lines
    where
        getSquare :: Char -> Maybe Bool
        getSquare ' ' = Nothing
        getSquare '.' = Just False
        getSquare '#' = Just True
        getSquare _   = error "Invalid Character"

parseMoves :: String -> [Move]
parseMoves xs = case span isDigit xs of
    (n, []) -> [Forward (read n)]
    (n, m:rest) -> Forward (read n) : case m of
        'R'  -> TurnRight : parseMoves rest
        'L'  -> TurnLeft  : parseMoves rest
        '\n' -> []
        c    -> error $ "Invalid Turn: " ++ show c

part1 :: Input -> Output1
part1 = solve p1Wrap

doMove :: (Pos -> Facing -> Map Pos Bool -> (Pos, Facing)) -> Map Pos Bool -> (Pos, Facing) -> Move -> (Pos, Facing)
doMove _        _ (p, f) TurnLeft    = (p, prev f)
doMove _        _ (p, f) TurnRight   = (p, next f)
doMove wrapFunc m (p, f) (Forward n) = moveForward wrapFunc n p f m

moveForward :: (Pos -> Facing -> Map Pos Bool -> (Pos, Facing)) -> Int -> Pos -> Facing -> Map Pos Bool -> (Pos, Facing)
moveForward _ 0 p f _ = (p, f)
moveForward wrapFunc n p@(x,y) f m = checkNext $ case f of
    North -> (x-1, y)
    South -> (x+1, y)
    West  -> (x, y-1)
    East  -> (x, y+1)
    where
        (newPos, newFacing) = wrapFunc p f m
        checkNext (x', y') = case Map.lookup (x', y') m of
            Nothing    -> if m Map.! newPos
                    then ((x,y), f)
                    else moveForward wrapFunc (n-1) newPos newFacing m
            Just True  -> ((x, y), f)
            Just False -> moveForward wrapFunc (n-1) (x', y') f m

p1Wrap :: Pos -> Facing -> Map Pos Bool -> (Pos, Facing)
p1Wrap (x,y) f m = (,f) $ case f of
    North -> (Set.findMax $ Set.map fst $ Set.filter ((y==) . snd) $ Map.keysSet m, y)
    South -> (Set.findMin $ Set.map fst $ Set.filter ((y==) . snd) $ Map.keysSet m, y)
    East  -> (x, Set.findMin $ Set.map snd $ Set.filter ((x==) . fst) $ Map.keysSet m)
    West  -> (x, Set.findMax $ Set.map snd $ Set.filter ((x==) . fst) $ Map.keysSet m)

getScore :: (Pos, Facing) -> Int
getScore ((x,y), f) = 1000 * (x+1) + 4 * (y+1) + fromEnum f

solve :: (Pos -> Facing -> Map Pos Bool -> (Pos, Facing)) -> (Map Pos Bool, [Move]) -> Int
solve wrapFunc (m, ms) = getScore $ foldl' (doMove wrapFunc m) (fst $ Map.findMin m, East) ms

part2 :: Map (Int, Facing) (Int, Facing) -> Int -> Input -> Output2
part2 edgeMap edgeLength = solve $ p2Wrap edgeMap edgeLength

p2Wrap :: Map (Int, Facing) (Int, Facing) -> Int -> Pos -> Facing -> Map Pos Bool -> (Pos, Facing)
p2Wrap edgeMap edgeLength p f m = (zippedPoints Map.! p, newFacing)
    where
        edges = getEdges edgeLength m
        invMap = Map.unions $ map (uncurry Map.fromSet . first (const . fst)) $ Map.toList edges
        myEdge = invMap Map.! p
        myEdgePoints = edges Map.! (myEdge, f)
        (newEdge, newFacing) = edgeMap Map.! (myEdge, f)
        newEdgePoints = edges Map.! (newEdge, next (next newFacing))
        zippedPoints = Map.fromAscList
                     $ zip (Set.toAscList myEdgePoints)
                     $ if shouldReverse f newFacing
                        then Set.toDescList newEdgePoints
                        else Set.toAscList newEdgePoints

getEdges :: Int -> Map Pos Bool -> Map (Int, Facing) (Set Pos)
getEdges edgeLength m = Map.unions
           $ zipWith (\x -> Map.mapKeysMonotonic (x,)) [1..]
           $ filter ((`Map.member` m) . Set.findMin . head . Map.elems)
                [Set.map (bimap (x+) (y+)) <$> Map.fromList [(East, eastEdge), (South, southEdge), (West, westEdge), (North, northEdge)]
                    | x <- [0,edgeLength..4*edgeLength], y <- [0,edgeLength..4*edgeLength]]
    where
        northEdge = Set.fromAscList [(0, y)  | y <- [0..edgeLength-1]]
        southEdge = Set.fromAscList [(edgeLength-1, y) | y <- [0..edgeLength-1]]
        westEdge  = Set.fromAscList [(x, 0)  | x <- [0..edgeLength-1]]
        eastEdge  = Set.fromAscList [(x, edgeLength-1) | x <- [0..edgeLength-1]]

cubeEdgeMap :: Map (Int, Facing) (Int, Facing)
cubeEdgeMap = Map.fromList [
    ((1, North), (6, East)),
    ((1, East),  (2, East)),
    ((1, South), (3, South)),
    ((1, West),  (4, East)),
    ((2, North), (6, North)),
    ((2, East),  (5, West)),
    ((2, South), (3, West)),
    ((2, West),  (1, West)),
    ((3, North), (1, North)),
    ((3, East),  (2, North)),
    ((3, South), (5, South)),
    ((3, West),  (4, South)),
    ((4, North), (3, East)),
    ((4, East),  (5, East)),
    ((4, South), (6, South)),
    ((4, West),  (1, East)),
    ((5, North), (3, North)),
    ((5, East),  (2, West)),
    ((5, South), (6, West)),
    ((5, West),  (4, West)),
    ((6, North), (4, North)),
    ((6, East),  (5, North)),
    ((6, South), (2, South)),
    ((6, West),  (1, South))
    ]

testCubeEdgeMap :: Map (Int, Facing) (Int, Facing)
testCubeEdgeMap = Map.fromList [
    ((1, North), (2, South)),
    ((1, East),  (6, West)),
    ((1, South), (4, South)),
    ((1, West),  (3, South)),
    ((2, North), (1, South)),
    ((2, East),  (3, East)),
    ((2, South), (5, North)),
    ((2, West),  (6, North)),
    ((3, North), (1, East)),
    ((3, East),  (4, East)),
    ((3, South), (5, East)),
    ((3, West),  (2, West)),
    ((4, North), (1, North)),
    ((4, East),  (6, South)),
    ((4, South), (5, South)),
    ((4, West),  (3, West)),
    ((5, North), (4, North)),
    ((5, East),  (6, East)),
    ((5, South), (2, North)),
    ((5, West),  (3, North)),
    ((6, North), (4, West)),
    ((6, East),  (1, West)),
    ((6, South), (2, East)),
    ((6, West),  (5, West))
    ]

shouldReverse :: Facing -> Facing -> Bool
shouldReverse North East = False
shouldReverse North West = True
shouldReverse South East = True
shouldReverse South West = False
shouldReverse f1 f2
    | f1 == f2 = False
    | f1 == next (next f2) = True
    | otherwise = shouldReverse f2 f1
