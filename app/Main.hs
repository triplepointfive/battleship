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

import Warships.BattleField
import Warships.Generator (brandNewField)

type Client = WS.Connection

instance Eq WS.Connection where
  _ == _ = False

data ServerState
  = ServerState
  { clients :: ![Client]
  , field :: !BattleField
  }

newServerState :: BattleField -> ServerState
newServerState = ServerState []

addClient :: Client -> ServerState -> ServerState
addClient client state = state { clients = client : clients state }

removeClient :: Client -> ServerState -> ServerState
removeClient client (ServerState _ f) = ServerState [] f

broadcast :: Text -> ServerState -> IO ()
broadcast message state = do
    T.putStrLn message
    forM_ (clients state) (`WS.sendTextData` message)

main :: IO ()
main = do
    field <- brandNewField
    state <- newMVar (newServerState field)
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    client <- WS.acceptRequest pending
    WS.forkPingThread client 30

    msg <- WS.receiveData client :: IO T.Text
    let disconnect = do
          -- Remove client and return new state
          s <- modifyMVar state $ \s ->
              let s' = removeClient client s in return (s', s')
          broadcast " disconnected" s

    flip finally disconnect $ do
        liftIO $ modifyMVar_ state $ \s -> do
            let s' = addClient client s
            broadcast (displayField (field s)) s'
            return s'
        talk client state

talk :: WS.Connection -> MVar ServerState -> IO ()
talk conn state = forever $ do
    msg <- T.unpack <$> WS.receiveData conn :: IO String
    s' <- readMVar state
    let f' = attack (read msg) (field s')

    if gameComplete f'
      then do
        f'' <- brandNewField
        modifyMVar_ state $ \s -> return $ s { field = f'' }
      else
        modifyMVar_ state $ \s -> return $ s { field = f' }

    liftIO $ readMVar state >>= broadcastField

gameComplete :: BattleField -> Bool
gameComplete = all (==0) . Map.elems . ships

broadcastField :: ServerState -> IO ()
broadcastField state@(ServerState _ field) =
  broadcast (displayField field) state

displayField :: BattleField -> T.Text
displayField bf = T.intercalate "" [ T.intercalate "" [ sc $ getCell (x, y) bf | y <- [0..height bf - 1] ] | x <- [0..width bf - 1] ]
  where
    sc Empty            = "E"
    sc Miss             = "M"
    sc (Ship Hidden _)  = "H"
    sc (Ship Injured _) = "I"
    sc (Ship Killed _)  = "K"
