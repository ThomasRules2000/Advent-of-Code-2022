module Util.Field where
import           Data.IntSet

data Field = Field { fieldName :: String
                   ,  fieldSet  :: IntSet
                   } deriving (Eq)

instance Show Field where
  show f = "\"" ++ fieldName f ++ "\""
