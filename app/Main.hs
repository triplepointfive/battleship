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

type Client = WS.Connection

instance Eq WS.Connection where
  _ == _ = False

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter (/= client)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients (`WS.sendTextData` message)

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    client <- WS.acceptRequest pending
    WS.forkPingThread client 30

    msg <- WS.receiveData client :: IO T.Text
    clients <- liftIO $ readMVar state
    let disconnect = do
          -- Remove client and return new state
          s <- modifyMVar state $ \s ->
              let s' = removeClient client s in return (s', s')
          broadcast " disconnected" s

    flip finally disconnect $ do
        liftIO $ modifyMVar_ state $ \s -> do
            let s' = addClient client s
            -- WS.sendTextData client $ "Welcome! Users: " `mappend` T.intercalate ", " (map fst s)
            -- broadcast (fst client `mappend` " joined") s'
            broadcast (displayField tF) s'
            return s'
        talk client state

talk :: WS.Connection -> MVar ServerState -> IO ()
talk conn state = forever $ do
    msg <- WS.receiveData conn :: IO T.Text
    liftIO $ readMVar state >>= broadcast (displayField tF)

displayField :: BattleField -> T.Text
displayField bf = T.intercalate "\n" [ T.intercalate "" [ sc $ getCell (x, y) bf | x <- [0..width bf - 1] ] | y <- [0..height bf - 1] ]
  where
    sc Empty            = "E"
    sc Miss             = "M"
    sc (Ship Hidden _)  = "H"
    sc (Ship Injured _) = "I"
    sc (Ship Killed _)  = "K"

tF :: BattleField
tF = BattleField 10 10 g2 s2
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
