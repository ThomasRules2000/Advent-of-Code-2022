module Days.Day02 where
import           Data.Bifunctor (bimap)
import           Data.Char      (ord)
import           Data.Mod       (Mod, unMod)
import qualified Program.RunDay as R (runDay)
import           System.Clock   (TimeSpec)
import           Util.Util      (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

type RPS = Mod 3

type Input = [(RPS, RPS)]

type Output1 = Int
type Output2 = Int

-- Convert the input chars to Z3 and use the following to align them (Rock=0, Paper=1, Scissors=2):
-- ord 'A' = 2 mod 3 ==> c - ord 'A' === (c + 2) mod 3
-- ord 'X' = 1 mod 3 ==> c - ord 'X' === (c + 1) mod 3
parser :: String -> Input
parser = map (bimap (+1) (+2) . listToTuple . map (fromIntegral . ord . head) . words) . lines

-- In Z3, RPS logic can be encoded by result = p1 - p2 (Win = 1, Draw = 0, Loss = -1)
-- We cycle this round and *3 so we can get our desired win=6, draw=3, loss=0, then add on the score for our play (again +1 because rock=1 in the scoring system)
part1 :: Input -> Output1
part1 = fromIntegral . sum . map (\(opp, me) -> 1 + unMod me + 3 * unMod (me - opp + 1))

-- Simple rearrangement means that res = me - opp ==> me = opp + res. As we have loss=0 here, we -1 to cycle this round as desired
part2 :: Input -> Output2
part2 = fromIntegral . sum . map (\(opp, res) -> 1 + unMod (opp + res - 1) + 3 * unMod res)
