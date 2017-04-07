{-# LANGUAGE RecordWildCards #-}
module Warships.Types where

import qualified Data.Map as Map

import Control.Monad.State

type ShipID = Int

data Cell = Empty | Hidden ShipID | Open | Injured ShipID | Killed ShipID
  deriving (Show, Eq)

hitCell :: Cell -> Cell
hitCell Empty = Open
hitCell (Hidden shipID) = Injured shipID
hitCell x = x


type Pos = (Int, Int)

type Grid = Map.Map Pos Cell

data Field = Field { fieldGrid :: !Grid }

-- fieldDone :: Field -> Bool
-- fieldDone Field{..} = all shipSunk fieldShips


data Action = Hit | Miss | Kill

click :: Pos -> Field -> (Action, Field)
click pos = runState (clickOn pos)

clickOn :: Pos -> State Field Action
clickOn = undefined

type GameID = Int

data GameState = GameState { field1 :: !Field, field2 :: !Field }

type Game = Map.Map GameID GameState
