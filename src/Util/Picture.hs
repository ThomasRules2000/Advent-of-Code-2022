module Util.Picture where
import           Data.Bifunctor (bimap)
import           Data.Matrix    (Matrix)
import qualified Data.Matrix    as Matrix
import           Data.Set       (Set)
import qualified Data.Set       as Set

newtype Picture = Picture {getMatrix :: Matrix Bool}
    deriving Eq

instance Show Picture where
    show picture = unlines (Matrix.toLists (boolChar <$> getMatrix picture))
        where boolChar :: Bool -> Char
              boolChar True  = '█'
              boolChar False = ' '

fromHashDot :: String -> Picture
fromHashDot = Picture . Matrix.fromLists . map (map (=='#')) . lines

fromSet :: Set (Int, Int) -> Picture
fromSet s = Picture $ Matrix.matrix rows cols (`Set.member` newSet)
    where
        minX = Set.findMin $ Set.map fst s
        minY = Set.findMin $ Set.map snd s
        newSet = Set.mapMonotonic (bimap ((1+) . subtract minX) ((1+) . subtract minY)) s

        rows = Set.findMax $ Set.map fst newSet
        cols = Set.findMax $ Set.map snd newSet
