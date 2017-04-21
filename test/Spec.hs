import Test.Hspec

import Warships.BattleField

import qualified Data.Map.Strict as Map

-- •....
-- .□.□.
-- .■...
-- .□.■.
-- ...□.
testField :: BattleField
testField = BattleField 5 5 g2 s2
  where
    s2 = Map.fromList
      [ (ShipID 1, 1)
      , (ShipID 2, 1)
      , (ShipID 3, 2)
      ]
    g2 = Map.fromList
      [ ((0,0), Miss)
      , ((3,1), Ship Hidden  (ShipID 1))
      , ((3,3), Ship Injured (ShipID 2))
      , ((3,4), Ship Hidden  (ShipID 2))
      , ((1,1), Ship Hidden  (ShipID 3))
      , ((1,2), Ship Injured (ShipID 3))
      , ((1,3), Ship Hidden  (ShipID 3))
      ]

main :: IO ()
main = hspec $
  describe "attack" $ do
    describe "on empty cell" $
      it "marks it missed" $
        let pos = (1, 0) in
            getCell pos (attack pos testField) `shouldBe` Miss
    describe "on miss cell" $
      it "does nothing" $
        let pos = (0, 0) in
            getCell pos (attack pos testField) `shouldBe` Miss
    describe "on an injured ships cell" $
      it "does nothing" $
        let pos = (3, 3) in
            getCell pos (attack pos testField) `shouldBe` Ship Injured (ShipID 2)
    describe "on a well feeling ships cell" $ do
      it "marks it injured if ship has extra well feeling cells" $
        let pos = (1, 1) in
            getCell pos (attack pos testField) `shouldBe` Ship Injured (ShipID 3)
      it "marks it killed if all ship cells are dead" $
        let pos = (3, 4) in
            getCell pos (attack pos testField) `shouldBe` Ship Killed (ShipID 2)
      it "marks remaining injured cells dead" $
        getCell (3, 3) (attack (3, 4) testField) `shouldBe` Ship Killed (ShipID 2)
      it "marks adjust cells dead" $
        getCell (4, 4) (attack (3, 4) testField) `shouldBe` Miss
    describe "on a one cell ship" $ do
      it "marks it dead" $
        let pos = (3, 1) in
            getCell pos (attack pos testField) `shouldBe` Ship Killed (ShipID 1)
      it "marks adjust cells dead" $
        getCell (2, 1) (attack (3, 1) testField) `shouldBe` Miss
