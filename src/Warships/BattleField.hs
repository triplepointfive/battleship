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
    Ship Hidden shipID -> do
      decreaseShipHealth shipID
      setCell (Ship Injured shipID) pos
      checkShip shipID
    _ -> return ()

decreaseShipHealth :: ShipID -> OnBattleField ()
decreaseShipHealth shipID = do
  field <- get
  put $ field { ships = Map.adjust pred shipID (ships field) }

getShipHealth :: ShipID -> OnBattleField Int
getShipHealth shipID = Map.findWithDefault defaultValue shipID . ships <$> get
  where
    defaultValue = error $ "ship " ++ show shipID ++ " couldn't be found"

checkShip :: ShipID -> OnBattleField ()
checkShip shipID = do
  shipHealth <- getShipHealth shipID
  when (shipHealth == 0) (killShip shipID)

killShip :: ShipID -> OnBattleField ()
killShip shipID = markShipDead shipID >> revealEmpty shipID

markShipDead :: ShipID -> OnBattleField ()
markShipDead shipID = onGrid (Map.map killShipCell)
  where
    killShipCell :: Cell -> Cell
    killShipCell (Ship Injured sID) | sID == shipID = Ship Killed shipID
    killShipCell cell = cell

revealEmpty :: ShipID -> OnBattleField ()
revealEmpty shipID = return ()

getCell :: Pos -> BattleField -> Cell
getCell pos field = Map.findWithDefault Empty pos (grid field)

setMiss :: Pos -> OnBattleField ()
setMiss = setCell Miss

setCell :: Cell -> Pos -> OnBattleField ()
setCell cell pos = onGrid (Map.insert pos cell)

onGrid :: (Grid -> Grid) -> OnBattleField ()
onGrid f = do
  field <- get
  put $ field { grid = f (grid field) }

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
