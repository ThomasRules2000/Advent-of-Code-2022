module Util.Picture where
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix

newtype Picture = Picture {getMatrix :: Matrix Bool}
    deriving Eq

instance Show Picture where
    show picture = unlines (Matrix.toLists (boolChar <$> getMatrix picture))
        where boolChar :: Bool -> Char
              boolChar True  = '█'
              boolChar False = ' '

fromHashDot :: String -> Picture
fromHashDot = Picture . Matrix.fromLists . map (map (=='#')) . lines