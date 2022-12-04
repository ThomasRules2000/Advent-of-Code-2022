{-# LANGUAGE ScopedTypeVariables #-}
module Program.TestDay where
import           Test.Hspec

testDay :: (Eq out1, Show out1, Eq out2, Show out2) => (String -> inp) -> (inp -> out1) -> (inp -> out2) -> out1 -> out2 -> String -> String -> Spec
testDay parser part1 part2 p1Ans p2Ans name file = do
    describe name $ do
        let inp = parser file
        it "Part 1" $ part1 inp `shouldBe` p1Ans
        it "Part 2" $ part2 inp `shouldBe` p2Ans
