module Communication
    (
    handleSendingMessage,
    sendMessage,
    sendMessageToAllBut,
    broadcast,
    monitorBroadcast
    ) where

import ServerData
import System.IO
import Control.Concurrent
import Control.Concurrent.Async
import Data.Map (Map)
import qualified Data.Map as Map
import DataStringUtils (replace)

-- handle translating a Message object into a string and sending it to a client
handleSendingMessage :: Server -> Client -> Message -> IO Bool
handleSendingMessage server client@Client{..} message =
  case message of
    Echo str -> output $ "SERVER/" ++ str
    Welcome name -> output $ "BIENVENUE/" ++ name
    ConnectNotification name -> output $ "CONNECTE/" ++ name
    DisconnectNotification name -> output $ "SORTI/" ++ name
    TellEveryone name txt -> output $ "CHAT/" ++ name ++ "/" ++ txt
    OpenSession level -> output $ "SESSION/" ++ level
    SumUpTurn problem scores -> output $ "TOUR/" ++ (problemToString problem) ++ "/" ++ scores
    ValidateSolution -> output $ "TUASTROUVE/"
    FirstSolutionNotification name nb_moves ->  output $ "ILATROUVE/" ++ name ++ "/" ++ nb_moves
    EndReflexion -> output "FINREFLEXION/"
    ValidateBid -> output "TUENCHERE/"
    FailureBid name -> output $ "ECHECENCHERE/" ++ name
    BidNotification name nb_moves -> output $ "ILENCHERE/" ++ name ++ "/" ++ nb_moves
    EndBiddingPhase name nb_moves -> output $ "FINENCHERE/" ++ name ++ "/" ++ nb_moves
    ProposedSolutionNotification name moves -> output $ "SASOLUTION/" ++ name ++ "/" ++ (movesToString moves)
    SolutionAccepted -> output  "BONNE/"
    SolutionRefused name -> output $ "MAUVAISE/" ++ name
    EndResolutionPhase -> output "FINRESO/"
    TimedOut name -> output $ "TROPLONG/" ++ name
    AnnounceWinner scores -> output $ "VAINQUEUR/" ++ scores
    _ -> output "ERROR : UNKNOWN MESSAGE.."
  where output s = do hPutStrLn cHandle s; return True

-- Note because we use data.matrix which is indexed from 1 we apply -1 here
problemToString :: Problem -> String
problemToString ((xr,yr), (xb,yb), (xj,yj), (xv,yv), (xc, yc, c)) =
  "(" ++ show (xr-1) ++ "," ++ show (yr-1) ++ ","
  ++ show (xb-1) ++ "," ++ show (yb-1) ++ ","
  ++ show (xj-1) ++ "," ++ show (yj-1) ++ ","
  ++ show (xv-1) ++ "," ++ show (yv-1) ++ ","
  ++ show (xc-1) ++ "," ++ show (yc-1) ++ "," ++ show c
  ++ ")"

movesToString :: SolutionMoves -> String
movesToString [] = ""
movesToString ((c,d):s) = replace "'" "" (show c) ++ replace "'" "" (show d) ++ (movesToString s)


-- take messages from the broascast channel and transmit them to the actual client channel
monitorBroadcast :: Chan Message -> MVar (Chan Message) -> IO()
monitorBroadcast bcChan sendChan = do
  loopMonitor
  where
    loopMonitor = do
      msg <- readChan bcChan
      sChan <- takeMVar sendChan
      writeChan sChan msg
      putMVar sendChan sChan
      loopMonitor

-- send message to single client
-- Note : {..} gives access to every field of Client such as clientSendChannel
sendMessage :: Client -> Message -> IO ()
sendMessage Client{..} msg = do
  c <- takeMVar cSendChannel
  writeChan c msg
  putMVar cSendChannel c


-- note : unused => don't wanna lock all the clients channel for an "almost" broadcast
-- so we broadcast every "general" kind of message and we'll filter within the monitor thread
-- if need be
sendMessageToAllBut :: Server -> ClientName -> Message -> IO ()
sendMessageToAllBut Server{..} name msg = do
  clientmap <- readMVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems (Map.delete name clientmap))

-- send message to all clients at once
broadcast :: Server -> Message -> IO ()
broadcast Server{..} msg = do
  writeChan broadcastChan msg
