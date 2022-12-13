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
import           Data.List                        (elemIndex, sortBy)
import           Data.Maybe                       (fromJust)
import           Data.Tuple.Extra                 (dupe)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 13 140

data Packet = I Int | L [Packet]
    deriving (Eq, Ord, Show)

type Input = [(Packet, Packet)]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = fromRight [] . parseOnly topParser . BS.pack

topParser :: Parser [(Packet, Packet)]
topParser = ((,) <$> packetParser <*> (endOfLine *> packetParser)) `sepBy` (endOfLine *> endOfLine)

packetParser :: Parser Packet
packetParser = L <$> (I <$> decimal <|> between (char '[') (char ']') packetParser) `sepBy` char ','

part1 :: Input -> Output1
part1 = sum . map fst . filter ((==LT) . snd) . zip [1..] . map (uncurry comp)

comp :: Packet -> Packet -> Ordering
comp (I n1) (I n2)
    | n1 > n2 = GT
    | n1 < n2 = LT
    | otherwise = EQ
comp (L ps1) (L ps2) = case filter (/=EQ) $ zipWith comp ps1 ps2 of
    [] | length ps1 > length ps2 -> GT
       | length ps2 > length ps1 -> LT
       | otherwise -> EQ
    (x:_) -> x
comp p1@(I _) p2 = comp (L [p1]) p2
comp p1 p2@(I _) = comp p1 (L [p2])


part2 :: Input -> Output2
part2 = uncurry (*)
      . bimap ((+1) . fromJust . elemIndex k1) ((+1) . fromJust . elemIndex k2)
      . dupe
      . sortBy comp
      . (++[k1, k2])
      . concatMap (\(p1, p2) -> [p1, p2])
    where
        k1 = L [L [I 2]]
        k2 = L [L [I 6]]
