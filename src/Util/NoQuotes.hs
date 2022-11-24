module Util.NoQuotes where

newtype NoQuotes = NoQuotes String
    deriving (Eq, Ord)

instance Show NoQuotes where 
    show (NoQuotes s) = s