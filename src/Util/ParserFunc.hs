{-# LANGUAGE MultiParamTypeClasses #-}
module Util.ParserFunc where
import           Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import qualified Data.ByteString.Char8            as BS

class ParserFunc f a where
    makeParser :: f -> String -> a

instance ParserFunc (String -> a) a where
    makeParser = id

instance ParserFunc (Parser a) a where
    makeParser p = (\(Right x) -> x) . parseOnly p . BS.pack
