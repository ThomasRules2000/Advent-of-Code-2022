module Util.HexTile where
import           Data.Bifunctor
import qualified Data.Set       as Set
import           Util.Coord

newtype HexTile = HexTile (Int, Int) deriving (Eq, Show, Ord)

east :: HexTile -> HexTile
east (HexTile (ew, nesw)) = HexTile (ew+1, nesw)

west :: HexTile -> HexTile
west (HexTile (ew, nesw)) = HexTile (ew-1, nesw)

northEast :: HexTile -> HexTile
northEast (HexTile (ew, nesw)) = HexTile (ew, nesw+1)

southWest :: HexTile -> HexTile
southWest (HexTile (ew, nesw)) = HexTile (ew, nesw-1)

northWest :: HexTile -> HexTile
northWest (HexTile (ew, nesw)) = HexTile (ew-1, nesw+1)

southEast :: HexTile -> HexTile
southEast (HexTile (ew, nesw)) = HexTile (ew+1, nesw-1)

zero :: HexTile
zero = HexTile (0, 0)

instance Coord HexTile where
    getNeighbours (HexTile (ew, nesw)) = Set.fromList $ map (HexTile . bimap (+ew) (+nesw)) [(0,0), (-1,1), (0,1), (1,0), (1,-1), (0,-1), (-1,0)]
    from2D (x,y) = HexTile (x,y)
