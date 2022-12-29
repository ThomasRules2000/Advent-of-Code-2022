module Days.Day19 where
import           Control.Monad               (when)
import           Control.Monad.ST.Strict     (runST)
import           Data.Biapplicative          (biliftA2)
import           Data.Maybe                  (catMaybes)
import           Data.Semigroup              (stimesMonoid)
import qualified Data.Vector.Unboxed.Mutable as MVector
import qualified Program.RunDay              as R (runDay)
import qualified Program.TestDay             as T (testDay)
import           System.Clock                (TimeSpec)
import           Test.Hspec                  (Spec)
import           Util.Util                   (toMaybe)

data Resources = Resources {
    ore      :: Int,
    clay     :: Int,
    obsidian :: Int,
    geode    :: Int
} deriving (Eq, Ord, Show)

instance Semigroup Resources where
    (Resources ore1 clay1 obs1 geo1) <> (Resources ore2 clay2 obs2 geo2) = Resources (ore1+ore2) (clay1+clay2) (obs1+obs2) (geo1+geo2)

instance Monoid Resources where
    mempty = Resources 0 0 0 0

data Blueprint = Blueprint {
    blueprintNum :: Int,
    oreBot       :: Int,
    clayBot      :: Int,
    obsidianBot  :: (Int, Int),
    geodeBot     :: (Int, Int)
} deriving (Eq, Ord, Show)

data World = World {
    resources :: Resources,
    bots      :: Resources
} deriving (Eq, Ord, Show)

toTuple :: World -> (Resources, Resources)
toTuple World{..} = (resources, bots)

instance Semigroup World where
    (World r1 b1) <> (World r2 b2) = World (r1 <> r2) (b1 <> b2)

instance Monoid World where
    mempty = World mempty mempty

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 33 3472

type Input = [Blueprint]

type Output1 = Int
type Output2 = Int

-- Initially we have 1 ore bot and nothing else
initialWorld :: World
initialWorld = World mempty (Resources 1 0 0 0)

parser :: String -> Input
parser = map go . lines
    where go s = Blueprint{..}
            where
                w = words s
                blueprintNum = read $ init $ w !! 1
                oreBot = read $ w !! 6
                clayBot = read $ w !! 12
                obsidianBot = (read $ w !! 18, read $ w !! 21)
                geodeBot = (read $ w !! 27, read $ w !! 30)

part1 :: Input -> Output1
part1 = sum . zipWith (*) [1..] . map (maxGeodes 24)

maxGeodes :: Int -> Blueprint -> Int
maxGeodes initialTime Blueprint{..} = runST $ do
    v <- MVector.replicate (initialTime + 1) (0,0)
    let go t World{..} = do
            maxGeos <- MVector.read v $ max 0 t
            if | t < 0 -> when (geoInfo > maxGeos) $ MVector.write v 0 geoInfo
               | (ore bots, obsidian bots) == geodeBot -> do
                    finalMaxGeos <- MVector.read v 0
                    let finalGeos = (geode resources + (t * geode bots) + ((t * (t+1)) `div` 2), geode bots + (t * (t+1)) `div` 2)
                    when (finalGeos > finalMaxGeos) $ MVector.write v 0 finalGeos
                -- I don't like this but it's needed to stop pruning too aggressively when it's better to make more obsidian bots first
               | otherwise -> when (uncurry (||) $ biliftA2 ((>=) . (+1)) (>) geoInfo maxGeos) $ do
                    when (geoInfo > maxGeos) $ MVector.write v t geoInfo
                    mapM_ (uncurry go) $ catMaybes [makeGeo, makeObs, makeClay, makeOre]
            where
                geoInfo = (geode resources, geode bots)
                makeOre = toMaybe (ore bots < maxReqOre)
                            (t-t', World{resources=resources <> stimesMonoid (min t t') bots <> mempty{ore= -oreBot},
                                         bots=bots <> mempty{ore=1}})
                    where t' = 1 + max 0 (((oreBot - ore resources - 1) `div` ore bots) + 1)
                makeClay = toMaybe (clay bots < snd obsidianBot)
                            (t-t', World{resources=resources <> stimesMonoid (min t t') bots <> mempty{ore= -clayBot},
                                         bots=bots <> mempty{clay=1}})
                    where t' = 1 + max 0 (((clayBot - ore resources - 1) `div` ore bots) + 1)
                makeObs = toMaybe (clay bots > 0 && obsidian bots < snd geodeBot)
                            (t-t', World{resources=resources <> stimesMonoid (min t t') bots <> mempty{ore= -fst obsidianBot, clay = -snd obsidianBot},
                                         bots=bots <> mempty{obsidian=1}})
                    where t' = 1 + maximum [0, ((fst obsidianBot - ore resources - 1) `div` ore bots) + 1,
                                            ((snd obsidianBot - clay resources - 1) `div` clay bots) + 1]
                makeGeo = toMaybe (obsidian bots > 0)
                            (t-t', World{resources=resources <> stimesMonoid (min t t') bots <> mempty{ore= -fst geodeBot, obsidian = -snd geodeBot},
                                         bots=bots <> mempty{geode=1}})
                    where t' = 1 + maximum [0, ((fst geodeBot - ore resources - 1) `div` ore bots) + 1,
                                            ((snd geodeBot - obsidian resources - 1) `div` obsidian bots) + 1]

    go initialTime initialWorld
    fst <$> MVector.read v 0
    where maxReqOre = maximum [clayBot, fst obsidianBot, fst geodeBot]

part2 :: Input -> Output2
part2 = product . take 3 . map (maxGeodes 32)
