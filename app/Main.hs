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
  { clients :: !(Map.Map PlayerID Player)
  , games   :: !(Map.Map GameID ())
  } deriving Show

-- | All messages that server can `broadcast` to clients.
data OutputMessage
  -- | Returns when a user searches for a game with specific ID, but it can't be found.
  = NotFoundGameID
  -- | Found game already has two players.
  | NoFreeSlots
  -- | User initialized new game and this is its ID.
  | NewGameID GameID
  -- | User joined a game and received their ID.
  | PlayerID PlayerID
  deriving Eq

instance Show OutputMessage where
  show NotFoundGameID  = "NFDGID"
  show NoFreeSlots     = "NFS"
  show (NewGameID gID) = "NGID " ++ show gID
  show (PlayerID pID)  = "PID " ++ show pID

-- | All messages that server accepts.
data IntputMessage
  -- | User initializes new game.
  = NewGame
  -- | User joins an existing game.
  | JoinGame GameID
  -- | User attacks enemy's field.
  | Attack
  deriving (Read, Show, Eq)

newServerState :: ServerState
newServerState = ServerState Map.empty Map.empty

addClient :: PlayerID -> Player -> ServerState -> ServerState
addClient pID player state
  = state { clients = Map.insert pID player (clients state) }

removeClient :: PlayerID -> ServerState -> ServerState
removeClient pID s = s { clients = Map.delete pID (clients s) }

broadcast :: Text -> ServerState -> IO ()
broadcast message ServerState{..} = do
  T.putStrLn message
  forM_ (map connection $ Map.elems clients) (`WS.sendTextData` message)

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    client <- WS.acceptRequest pending
    pID <- randomIO
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

broadcastMessage :: OutputMessage -> ServerState -> IO ()
broadcastMessage m = broadcast (T.pack $ show m)

broadcastPlayer :: ServerState -> PlayerID -> IO ()
broadcastPlayer ServerState{..} pID =
  case Map.lookup pID clients of
    Just Player{..} -> do
      WS.sendTextData connection (T.pack $ "PID " ++ show pID)
      WS.sendTextData connection (T.concat ["OWN ", displayOwnField field])
    Nothing -> return ()

talk :: PlayerID -> WS.Connection -> MVar ServerState -> IO ()
talk pID conn state = forever $ do
    msg <- T.unpack <$> WS.receiveData conn :: IO String
    s' <- readMVar state
    print msg
    print s'
    case read msg of
      NewGame -> do
        gameID <- randomIO
        modifyMVar_ state $ \s -> return $ s { games = Map.insert gameID () (games s) }
        broadcastMessage (NewGameID gameID) s'
      -- JoinGame gameID -> do
        -- return ()
      -- Attack -> do
        -- let f' = attack (read msg) (field s')

        -- if gameComplete f'
          -- then do
            -- f'' <- brandNewField
            -- modifyMVar_ state $ \s -> return $ s { field = f'' }
          -- else
            -- modifyMVar_ state $ \s -> return $ s { field = f' }

        -- liftIO $ readMVar state >>= broadcastField

gameComplete :: BattleField -> Bool
gameComplete = all (==0) . Map.elems . ships

-- broadcastField :: ServerState -> IO ()
-- broadcastField state@ServerState{..} =
  -- broadcast (displayField field) state

displayOwnField :: BattleField -> T.Text
displayOwnField bf = T.intercalate "" [ T.intercalate "" [ sc $ getCell (x, y) bf | y <- [0..height bf - 1] ] | x <- [0..width bf - 1] ]
  where
    sc Empty            = "E"
    sc Miss             = "M"
    sc (Ship Hidden _)  = "H"
    sc (Ship Injured _) = "I"
    sc (Ship Killed _)  = "K"
