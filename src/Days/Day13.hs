module Days.Day13 where
import           Control.Applicative              ((<|>))
import           Control.Applicative.Combinators  (between, sepBy)
import           Data.Attoparsec.ByteString.Char8 (Parser, char, decimal,
                                                   endOfLine, parseOnly)
import qualified Data.ByteString.Char8            as BS
import           Data.Either                      (fromRight)
import qualified Program.RunDay                   as R (runDay)
import qualified Program.TestDay                  as T (testDay)
import           System.Clock                     (TimeSpec)
import           Test.Hspec                       (Spec)

import           Data.Bifunctor                   (bimap)
import           Data.List                        (elemIndex, sort)
import           Data.Maybe                       (fromJust)
import           Data.Tuple.Extra                 (dupe)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 13 140

data Packet = I Int | L [Packet]
    deriving (Eq, Show)

instance Ord Packet where
    compare (I n1) (I n2) = compare n1 n2
    compare (L p1) (L p2) = case filter (/=EQ) $ zipWith compare p1 p2 of
        [] | length p1 > length p2 -> GT
           | length p2 > length p1 -> LT
           | otherwise -> EQ
        (x:_) -> x
    compare p1@(I _) p2 = compare (L [p1]) p2
    compare p1 p2@(I _) = compare p1 (L [p2])

type Input = [(Packet, Packet)]

type Output1 = Int
type Output2 = Int

parser :: Parser Input
parser = ((,) <$> packetParser <*> (endOfLine *> packetParser)) `sepBy` (endOfLine *> endOfLine)

packetParser :: Parser Packet
packetParser = L <$> (I <$> decimal <|> between (char '[') (char ']') packetParser) `sepBy` char ','

part1 :: Input -> Output1
part1 = sum . map fst . filter (uncurry (<) . snd) . zip [1..]

part2 :: Input -> Output2
part2 = uncurry (*)
      . bimap ((+1) . fromJust . elemIndex k1) ((+1) . fromJust . elemIndex k2)
      . dupe
      . sort
      . (++[k1, k2])
      . concatMap (\(p1, p2) -> [p1, p2])
    where
        k1 = L [L [I 2]]
        k2 = L [L [I 6]]
