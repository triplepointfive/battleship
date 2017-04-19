module Warships.BattleField where

import qualified Data.Map.Strict as Map
import Data.List (intercalate)

import Control.Monad.State

type Pos = (Int, Int)

newtype ShipID = ShipID Int
  deriving (Show, Ord, Eq)

data ShipState = Hidden | Injured | Killed
  deriving (Show, Ord, Eq)

data Cell = Empty | Miss | Ship ShipState ShipID
  deriving (Show, Ord, Eq)

type Grid = Map.Map Pos Cell

data BattleField
  = BattleField
  { grid  :: !Grid
  , ships :: !(Map.Map ShipID Int)
  }
  deriving (Show, Eq)

type OnBattleField = State BattleField

attack :: Pos -> BattleField -> BattleField
attack pos field = flip execState field $ do
  cell <- getCell pos <$> get
  case cell of
    Empty -> setMiss pos
    Miss  -> return ()

getCell :: Pos -> BattleField -> Cell
getCell pos field = Map.findWithDefault Empty pos (grid field)

setMiss :: Pos -> State BattleField ()
setMiss = setCell Miss

setCell :: Cell -> Pos -> OnBattleField ()
setCell cell pos = modify insertCell
  where
    insertCell :: BattleField -> BattleField
    insertCell field
      = field { grid = Map.insert pos cell (grid field) }

bf = BattleField g2 s2
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
