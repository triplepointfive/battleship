{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- import Data.Char (isPunctuation, isSpace)
-- import Data.List (intercalate)
-- import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (intercalate)

import qualified Network.WebSockets as WS

import System.Random (randomIO)

import Warships.BattleField
import Warships.Generator (brandNewField)

type PlayerID = Int

instance Show WS.Connection where
  show _ = "Connection"

data Player
  = Player
  { connection :: !WS.Connection
  , field      :: !BattleField
  } deriving Show

type GameID = Int

data ServerState
  = ServerState
  { players :: !(Map.Map PlayerID Player)
  } deriving Show

-- | All messages that server can `broadcast` to players.
data OutputMessage
  -- | Returns when a user searches for a game with specific ID, but it can't be found.
  = NotFoundGameID
  -- | Found game already has two players.
  | NoFreeSlots
  -- | User joined a game and received their ID.
  | PlayerID PlayerID
  | Own BattleField
  | Enemy BattleField
  deriving Eq

instance Show OutputMessage where
  show NotFoundGameID = "NotFoundGameID"
  show NoFreeSlots = "NoFreeSlots"
  show (PlayerID pID) = "PlayerID " ++ show pID
  show (Own bf) = "Own " ++ displayOwnField bf
  show (Enemy bf) = "Enemy " ++ displayEnemysField bf

-- | All messages that server accepts.
data IntputMessage
  -- | User joins an existing game.
  = JoinGame PlayerID
  -- | User attacks enemy's field.
  | Attack
  deriving (Read, Show, Eq)

getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault name defaultValue = do
    x <- lookupEnv name
    return $ fromMaybe defaultValue x

newServerState :: ServerState
newServerState = ServerState Map.empty

addClient :: PlayerID -> Player -> ServerState -> ServerState
addClient pID player state
  = state { players = Map.insert pID player (players state) }

removeClient :: PlayerID -> ServerState -> ServerState
removeClient pID s = s { players = Map.delete pID (players s) }

broadcast :: Text -> ServerState -> IO ()
broadcast message ServerState{..} = do
  T.putStrLn message
  forM_ (map connection $ Map.elems players) (`WS.sendTextData` message)

main :: IO ()
main = do
  state <- newMVar newServerState
  port <- read <$> getEnvWithDefault "PORT" "3636" :: IO Int
  host <- getEnvWithDefault "HOST" "0.0.0.0"
  putStrLn ("Running server on " ++ host ++ ":" ++ show port)
  WS.runServer host port $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    client <- WS.acceptRequest pending
    pID <- abs <$> randomIO
    WS.forkPingThread client 30

    msg <- WS.receiveData client :: IO T.Text
    let disconnect = modifyMVar_ state $ \s -> return (removeClient pID s)

    flip finally disconnect $ do
      liftIO $ modifyMVar_ state $ \s -> do
        field <- brandNewField
        return (addClient pID (Player client field) s)
      s <- readMVar state
      print s
      broadcastPlayer s pID
      talk pID client state

broadcastM :: OutputMessage -> WS.Connection -> IO ()
broadcastM msg conn = do
  T.putStrLn (T.pack (show msg))
  WS.sendTextData conn (T.pack (show msg))

broadcastPlayer :: ServerState -> PlayerID -> IO ()
broadcastPlayer ServerState{..} pID =
  case Map.lookup pID players of
    Just Player{..} -> do
      broadcastM (PlayerID pID) connection
      broadcastM (Own field) connection
    Nothing -> return ()

talk :: PlayerID -> WS.Connection -> MVar ServerState -> IO ()
talk pID conn state = forever $ do
    msg <- T.unpack <$> WS.receiveData conn :: IO String
    s' <- readMVar state
    print msg
    print s'
    case read msg of
      -- NewGame -> do
        -- gameID <- randomIO
        -- modifyMVar_ state $ \s -> return $ s { games = Map.insert gameID () (games s) }
        -- broadcastMessage (NewGameID gameID) s'
      JoinGame gID ->
        case Map.lookup gID (players s') of
          Just (Player c f) -> do
            broadcastM (Enemy f) conn
            broadcastM (Enemy (field $ fromJust $ Map.lookup pID (players s'))) c
          Nothing ->
            broadcastM NotFoundGameID conn

      -- Attack -> do
        -- let f' = attack (read msg) (field s')

        -- if gameComplete f'
          -- then do
            -- f'' <- brandNewField
            -- modifyMVar_ state $ \s -> return $ s { field = f'' }
          -- else
            -- modifyMVar_ state $ \s -> return $ s { field = f' }

        -- liftIO $ readMVar state >>= broadcastField

-- gameComplete :: BattleField -> Bool
-- gameComplete = all (==0) . Map.elems . ships

-- broadcastField :: ServerState -> IO ()
-- broadcastField state@ServerState{..} =
  -- broadcast (displayField field) state

displayOwnField :: BattleField -> String
displayOwnField = displayField "H"

displayEnemysField :: BattleField -> String
displayEnemysField = displayField "E"

displayField :: String -> BattleField -> String
displayField hiddenShip bf = intercalate "" [ intercalate "" [ sc $ getCell (x, y) bf | y <- [0..height bf - 1] ] | x <- [0..width bf - 1] ]
  where
    sc Empty            = "E"
    sc Miss             = "M"
    sc (Ship Hidden _)  = hiddenShip
    sc (Ship Injured _) = "I"
    sc (Ship Killed _)  = "K"
