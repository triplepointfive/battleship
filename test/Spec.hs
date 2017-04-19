import Test.Hspec

import Warships.BattleField

import qualified Data.Map.Strict as Map

testField :: BattleField
testField = BattleField g2 s2
  where
    s2 = Map.fromList
      [ (ShipID 1, 1)
      , (ShipID 2, 2)
      ]
    g2 = Map.fromList
      [ ((1,1), Ship Hidden (ShipID 1))
      , ((3,4), Ship Injured (ShipID 2))
      , ((3,3), Ship Hidden (ShipID 2))
      ]

main :: IO ()
main = hspec $ do
  describe "attack" $ do
    describe "on empty cell" $ do
      it "marks it missed" $
        let pos = (0, 0) in
            getCell pos (attack pos testField) `shouldBe` Miss
    describe "on miss cell" $ do
      it "does nothing" $
        let pos = (0, 0) in
            getCell pos (attack pos testField) `shouldBe` Miss
