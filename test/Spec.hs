import Test.Hspec

import Warships.Types

main :: IO ()
main = hspec $ do
  describe "hitCell" $ do
    it "Cell becomes miss if was empty" $ do
      hitCell Empty `shouldBe` Miss
