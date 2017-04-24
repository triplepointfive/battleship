{-# LANGUAGE RecordWildCards #-}
module Warships.Generator where

import Control.Monad.State
import System.Random

import qualified Data.Map.Strict as Map

import Warships.BattleField

import Data.List (intercalate)

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

generateField :: StdGen -> BattleField
generateField = evalState buildField

buildField :: Rnd BattleField
buildField = foldM addShip emptyField fieldShips

addShip :: BattleField -> RawShip -> Rnd BattleField
addShip field@BattleField{..} ship = do
  pos <- newPosition ship
  dir <- newDirection

  return $ field { grid = insertShip grid (produceShip pos dir ship) }

insertShip :: Grid -> [Pos] -> Grid
insertShip = foldl (\grid p -> Map.insert p (Ship Hidden (ShipID 0)) grid)

produceShip :: Pos -> Direction -> RawShip -> [Pos]
produceShip pos Horizontal ship = take ship $ iterate (\(x,y) -> (x + 1, y)) pos
produceShip pos Vertical   ship = take ship $ iterate (\(x,y) -> (x, y + 1)) pos

emptyField :: BattleField
emptyField = BattleField 10 10 Map.empty Map.empty

newDirection :: Rnd Direction
newDirection = (fst . random) <$> withSeed

newPosition :: RawShip -> Rnd Pos
newPosition ship = do
  a <- fst . randomR (0, 10 - ship) <$> withSeed
  b <- fst . randomR (0, 10 - ship) <$> withSeed
  return (a, b)

withSeed :: Rnd StdGen
withSeed = modify (snd . next) >> get

type RawShip = Int

fieldShips :: [RawShip]
fieldShips = [4, 3, 3, 2, 2, 2, 1, 1, 1, 1]

displayField :: BattleField -> String
displayField bf = intercalate "\n" [ intercalate "" [ sc $ getCell (x, y) bf | y <- [0..height bf - 1] ] | x <- [0..width bf - 1] ]
  where
    sc Empty            = " "
    sc Miss             = "."
    sc (Ship Hidden _)  = "H"
    sc (Ship Injured _) = "I"
    sc (Ship Killed _)  = "K"
