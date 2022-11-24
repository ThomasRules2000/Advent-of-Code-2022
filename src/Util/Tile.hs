module Util.Tile where
import           Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Data.Vector (Vector)
import qualified Data.Vector as Vec

data Tile = Tile {
  tileNum    :: Int,
  tileMatrix :: Matrix Bool
  } deriving (Eq)

instance Show Tile where
  show (Tile n matrix) = "\nTile " ++ show n ++ ":\n" ++ ppMatrix matrix

instance Ord Tile where
  t1 <= t2 = tileNum t1 <= tileNum t2

boolChar :: Bool -> Char
boolChar True  = '#'
boolChar False = '.'

ppMatrix :: Matrix Bool -> String
ppMatrix matrix = unlines (Matrix.toLists (boolChar <$> matrix))

getTop :: Tile -> Vector Bool
getTop = Matrix.getRow 1 . tileMatrix

getBottom :: Tile -> Vector Bool
getBottom t = let m = tileMatrix t in Matrix.getRow (Matrix.nrows m) m

getLeft :: Tile -> Vector Bool
getLeft = Matrix.getCol 1 . tileMatrix

getRight :: Tile -> Vector Bool
getRight t = let m = tileMatrix t in Matrix.getCol (Matrix.ncols m) m

getSides :: Tile -> Set (Vector Bool)
getSides t = Set.fromList [getTop t, getBottom t, getLeft t, getRight t]

getSidePerms :: Tile -> Set (Vector Bool)
getSidePerms t = Set.union sides $ Set.map Vec.reverse sides
  where sides = getSides t

getInside :: Tile -> Matrix Bool
getInside t = Matrix.submatrix 2 (Matrix.nrows matrix - 1) 2 (Matrix.ncols matrix - 1) matrix
  where matrix = tileMatrix t

empty :: Tile
empty = Tile 0 $ Matrix.fromList 10 10 $ replicate 100 False

flipH :: Matrix a -> Matrix a
flipH = Matrix.fromLists . reverse . Matrix.toLists

flipV :: Matrix a -> Matrix a
flipV = Matrix.fromLists . map reverse . Matrix.toLists

rotateRight :: Matrix a -> Matrix a
rotateRight = flipH . Matrix.transpose

rotateLeft :: Matrix a -> Matrix a
rotateLeft = flipV . Matrix.transpose

getDihedralTile :: Tile -> [Tile]
getDihedralTile t = map (Tile $ tileNum t) $ getDihedralGroup $ tileMatrix t

getDihedralGroup :: Matrix a -> [Matrix a]
getDihedralGroup m = [m,    flipH m,
                      m90,  flipH m90,
                      m180, flipH m180,
                      m270, flipH m270]
  where m90  = rotateRight m
        m180 = rotateRight m90
        m270 = rotateLeft m
