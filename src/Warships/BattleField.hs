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
  { width  :: !Int
  , height :: !Int
  , grid   :: !Grid
  , ships  :: !(Map.Map ShipID Int)
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
  where
    markShipDead :: ShipID -> OnBattleField ()
    markShipDead sID = onGrid (Map.map killShipCell)
      where
        killShipCell :: Cell -> Cell
        killShipCell cell
          | sameShip cell sID = Ship Killed sID
          | otherwise         = cell

    revealEmpty :: ShipID -> OnBattleField ()
    revealEmpty sID = onGrid $ \g ->
      let shipCells   = Map.keys $ Map.filter (`sameShip` sID) g
          adjustCells = concatMap adjustPositions shipCells
       in
       foldl (flip (Map.alter cs)) g adjustCells
      where
        cs :: Maybe Cell -> Maybe Cell
        cs Nothing = Just Miss
        cs (Just Empty) = Just Miss
        cs c = c

adjustPositions :: Pos -> [Pos]
adjustPositions (x, y) = filter (/= (x,y))
  [(x + i, y + j) | i <- [-1,0,1], j <- [-1,0,1]]

sameShip :: Cell -> ShipID -> Bool
sameShip (Ship _ sID) shipID = sID == shipID
sameShip _ _ = False

getCell :: Pos -> BattleField -> Cell
getCell pos field = Map.findWithDefault Empty pos (grid field)

setMiss :: Pos -> OnBattleField ()
setMiss = setCell Miss

setCell :: Cell -> Pos -> OnBattleField ()
setCell cell pos = onGrid (Map.insert pos cell)

onGrid :: (Grid -> Grid) -> OnBattleField ()
onGrid f = modify (\ field -> field { grid = f (grid field) })

displayField :: BattleField -> IO ()
displayField bf = putStrLn mapp
  where
    mapp = intercalate "\n" [ intercalate "" [ sc $ getCell (x, y) bf | x <- [0..width bf - 1] ] | y <- [0..height bf - 1] ]
    sc Empty            = "."
    sc Miss             = "•"
    sc (Ship Hidden _)  = "□"
    sc (Ship Injured _) = "■"
    sc (Ship Killed _)  = "x"

tF :: BattleField
tF = BattleField 5 5 g2 s2
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
