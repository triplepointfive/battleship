{-# LANGUAGE RecordWildCards #-}
module Warships.Types where

import qualified Data.Map as Map

type ShipID = Int

data Cell = Empty | HasShip ShipID | Miss | Hit ShipID | Killed ShipID
  deriving (Show, Eq)

hitCell :: Cell -> Cell
hitCell Empty = Miss
hitCell (HasShip shipID) = Hit shipID
hitCell x = x


type Grid = Map.Map (Int, Int) Cell

data Field = Field { fieldGrid :: !Grid }

-- fieldDone :: Field -> Bool
-- fieldDone Field{..} = all shipSunk fieldShips





type GameID = Int

data GameState = GameState { field1 :: !Field, field2 :: !Field }

type Game = Map.Map GameID GameState
