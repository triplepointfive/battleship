{-# LANGUAGE RecordWildCards #-}
module Warships.Generator
( brandNewField
) where

import Control.Monad.State
import System.Random

import qualified Data.Map.Strict as Map

import Warships.BattleField

import Data.List (intercalate, intersect)

data Direction = Horizontal | Vertical
  deriving (Show, Eq)

instance Random Direction where
  random seed | i `mod` 2 == 0 = (Horizontal, g)
              | otherwise      = (Vertical, g)
    where (i, g) = next seed

  randomR (lo, hi) seed | lo == hi  = (lo, snd (next seed))
                        | otherwise = random seed

type Rnd = State StdGen

fieldValid :: BattleField -> Bool
fieldValid BattleField{..} = gridBoundariesValid
  where
    gridBoundariesValid = all inRange (Map.keys grid)
      where inRange (x, y) = x >= 0 && y >= 0 && x < height && y < width

brandNewField :: IO BattleField
brandNewField = generateField . mkStdGen <$> randomIO

generateField :: StdGen -> BattleField
generateField = evalState buildField

buildField :: Rnd BattleField
buildField = foldM addShip emptyField fieldShips

addShip :: BattleField -> Int -> Rnd BattleField
addShip field@BattleField{..} shipLength = do
  pos <- newPosition shipLength
  dir <- newDirection
  shipID <- newShipID

  let ship = produceShip pos dir shipLength

  if ship `intersects` grid
     then addShip field shipLength
     else return $ field
            { grid  = insertShip shipID grid ship
            , ships = Map.insert shipID shipLength ships
            }

intersects :: [Pos] -> Grid -> Bool
intersects positions grid = not $ null $ intersect withAdjust (Map.keys grid)
  where
    withAdjust = positions ++ concatMap adjustPositions positions

newShipID :: Rnd ShipID
newShipID = ShipID . fst . random <$> withSeed

insertShip :: ShipID -> Grid -> [Pos] -> Grid
insertShip shipID = foldl (\grid p -> Map.insert p (Ship Hidden shipID) grid)

produceShip :: Pos -> Direction -> Int -> [Pos]
produceShip pos Horizontal shipLength = take shipLength $ iterate (\(x,y) -> (x + 1, y)) pos
produceShip pos Vertical   shipLength = take shipLength $ iterate (\(x,y) -> (x, y + 1)) pos

emptyField :: BattleField
emptyField = BattleField 10 10 Map.empty Map.empty

newDirection :: Rnd Direction
newDirection = (fst . random) <$> withSeed

newPosition :: Int -> Rnd Pos
newPosition shipLength = do
  a <- fst . randomR (0, 10 - shipLength) <$> withSeed
  b <- fst . randomR (0, 10 - shipLength) <$> withSeed
  return (a, b)

withSeed :: Rnd StdGen
withSeed = modify (snd . next) >> get

fieldShips :: [Int]
fieldShips = [4, 3, 3, 2, 2, 2, 1, 1, 1, 1]
