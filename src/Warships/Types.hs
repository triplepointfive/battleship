{-# LANGUAGE RecordWildCards #-}
module Warships.Types where

import qualified Data.Map as Map

import Control.Monad.State

import Warships.BattleField

data Field = Field { fieldGrid :: !Grid }
  deriving (Show, Eq)

-- fieldDone :: Field -> Bool
-- fieldDone Field{..} = all shipSunk fieldShips

data Action = Hit | Miss | Kill

click :: Pos -> Field -> (Action, Field)
click pos = runState (clickOn pos)

clickOn :: Pos -> State Field Action
clickOn = undefined

newtype PlayerTurn = PlayerTurn PlayerID
  deriving (Show, Eq)

data Game
  = PreparingGame (Maybe Field, Maybe Field)
  | ReadyGame PlayerTurn Field Field
  | CompleteGame Field Field
  deriving (Show, Eq)

-- | Identity of a player.
data PlayerID = Player1 | Player2
  deriving (Show, Eq, Ord)

-- | Identity of a game.
newtype GameID = GameID Int
  deriving (Show, Eq, Ord)

instance Enum GameID where
  succ (GameID i) = GameID (succ i)

-- | Basic auth credentials.
newtype Credentials = Credentials (PlayerID, GameID)
  deriving (Show, Eq)

-- | Any action player might'd like to do.
data PlayerAction
  = NewGame
  | JoinGame GameID
  | RequestGame GameID
  | Attack Credentials Pos
  deriving (Show, Eq)

-- | Responses to plaeyr's requests.
data PlayerResponse
  -- | A response for either new or joined game.
  = GameJoined Credentials Game
  -- | Current state of a game.
  | GameField Game
  | ErrorMessage String

data Repository = Repository GameID (Map.Map GameID Game)

startNewGame :: State Repository (GameID, Game)
startNewGame = do
  Repository lastID rep <- get
  let newGameID = succ lastID
      newGame   = PreparingGame (Nothing, Nothing)
  put $ Repository newGameID
        (Map.insert newGameID newGame rep)
  return (newGameID, newGame)

getGame :: GameID -> State Repository (Either String Game)
getGame = undefined

act :: PlayerAction -> State Repository PlayerResponse
act NewGame = do
  (gameID, game) <- startNewGame
  return $ GameJoined (Credentials (Player1, gameID)) game
act (JoinGame gameID) = do
  foundGame <- getGame gameID
  return $ case foundGame of
    Left msg -> ErrorMessage msg
    Right game -> GameField game
