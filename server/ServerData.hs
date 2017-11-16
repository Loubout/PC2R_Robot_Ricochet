module ServerData (
ClientName,
Wall,
GameLevel,
Problem,
NbMoves,
SingleMove,
SolutionMoves,
Report,
Client(..),
Message(..),
Phase(..),
Server(..),
Session(..),
Turn(..),
BidOutcome(..),
ResolutionType(..),
newServer,
newClient,
newSession,
newTurn,
scoreboardToString,
showPhase,
objective
) where


import System.IO
import Network
import Level
import Control.Concurrent
import Control.Concurrent.Async
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Text.Regex.Posix
import Control.Concurrent.Timer -- handle game phase timers
import DataStringUtils (escapeRe, replace)


{- CLIENT DATA -}
-- need to elaborate these types
type ClientName = String

type Wall = (Int, Int, Char) -- Char should be within {H,B,G,D}


type GameLevel = [Wall]

-- TO REWORK
type LevelStr = String


{-
  WARNING : since we use Data.Matrix on the server side which is indexed from 1
          here we'll use the same system
          however when sending the problem to the client, every coordinate will receive -1
-}
type Problem = ((Int,Int), (Int,Int), (Int,Int), (Int,Int), (Int,Int,Char)) -- initial positions of the robots


type NbMoves = String

type SingleMove = (Char, Char)

type SolutionMoves = [SingleMove]

type Report = String

{- Message sent from server to client -}
data Message = Welcome ClientName {- BIENVENUE/user - -}
              | ConnectNotification ClientName {- CONNECTE/user -}
              | DisconnectNotification ClientName {- SORTI/user -}
              | OpenSession LevelStr {- SESSION/plateau -}
              | AnnounceWinner Report {- VAINQUEUR/bilan -} -- CHANGE IN PROTOCAL SEND REPORT INSTEAD OF WINNER NAME
              | SumUpTurn Problem Report {- TOUR/enigme/bilan -}
              | ValidateSolution {- TUASTROUVE/ -}
              | FirstSolutionNotification ClientName NbMoves {- ILATROUVE/user/coups -}
              | EndReflexion {- FINREFLEXION/ -}
              | ValidateBid {- TUENCHERE/ -}
              | FailureBid ClientName {- ECHECENCHERE/user -}
              | BidNotification ClientName NbMoves {- ILENCHERE/user/coups -}
              | EndBiddingPhase ClientName NbMoves {- FINENCHERE/user/coups  -}
              | ProposedSolutionNotification ClientName SolutionMoves {- SASOLUTION/user/deplacements/ -}
              | SolutionAccepted {- BONNE/ -}
              | SolutionRefused ClientName {- MAUVAISE/user/ -}
              | EndResolutionPhase {- FINRESO/ -}
              | TimedOut ClientName {- TROPLONG/user -}
              -- EXTENSIONS
              | TellEveryone ClientName String {- CHAT/user/message -}
              | Echo String -- for debug purpose or send info message from the server
              deriving (Show)


data Phase = ReflectionPhase | BiddingPhase | ResolutionPhase deriving(Eq, Enum)

-- used to indicate the outcome of a bidding operation and send the correct error message
data BidOutcome = OkBid | BiddingPhaseOver | IncoherentBid ClientName | InvalidParticipant | InvalidNbMoves

-- used to indicate the reason why a resolution phase failed
data ResolutionType = PreviousTimeOut | BadSolution | FirstAttempt | TooManyMoves

-- unused show function
showPhase :: Phase -> String
showPhase ReflectionPhase = "Reflection"
showPhase BiddingPhase = "Bidding"
showPhase ResolutionPhase = "Resolution"

-- Store the current state of the server
data Server = Server {
  clients :: MVar (Map ClientName Client),
  broadcastChan :: Chan Message, -- broadcast channel which is duplicated to each client
  session :: MVar Session,
  currentTurn :: MVar Turn
}



-- NEED TO IMPLEMENT THE # OF TURN AND GOAL SCORE
data Session = Session {
  scoreboard :: Map ClientName Int, -- needs to be protected cuz u add new players
  level :: LevelStr, -- CAREFUL WITH THIS
  countTurn :: Int -- basic counter for turns
}

objective :: Int
objective = 10

data Client = Client {
  cName :: ClientName,
  cHandle :: Handle,
  cBroadcastChan :: Chan Message, -- broadcast channel wich is a duplicate from the server's bc chan
  cSendChannel :: MVar (Chan Message) -- actual send channel that receive from broadcast and network
}


data Turn = Turn {
  problem :: Problem,
  phase :: Phase,
  participants :: [ClientName],
  biddings :: Map ClientName Int,
  phaseTimer :: TimerIO,
  activePlayer :: (ClientName, Int)

}

newClient :: ClientName -> Chan Message-> Handle -> IO Client
newClient name chan handle = do
  s <- newChan
  sbis <- newMVar s
  c <- dupChan chan
  return Client {
    cName = name,
    cHandle = handle,
    cBroadcastChan = c,
    cSendChannel = sbis
  }

newServer :: IO Server
newServer = do
  chan <- newChan
  c <- newMVar (Map.empty)
  s <- newEmptyMVar
  t <- newEmptyMVar
  return Server {
      clients = c,
      broadcastChan = chan,
      session = s,
      currentTurn = t
  }

-- initialize a new session with every score at 0
newSession ::Server -> IO Session
newSession server@Server{..} = do
  clientmap <- readMVar clients
  let sb =  Map.fromList [(n,0) | n <- Map.keys clientmap]-- this creates no IO so we use let
  lvl <- getGridFromFile "./test_grid.txt" -- this performs an IO aciton so we use <-
  return Session {
    scoreboard = sb,
    level = lvl,
    countTurn = 0
  }
        -- CAREFUL
newTurn :: LevelStr -> Server -> IO Turn
newTurn level serv@Server{..} = do
  t <- newTimer
  clientmap <- readMVar clients
  -- (xr,yr,xb,yb,xj,yj,xv,yv,xc,yc,c)
  pb <- generateRandomProblem level
  let player_names = Map.keys clientmap -- otherwise names are registered as "<name>\r"
      player_bids = Map.empty

  -- need to generate enigma
  return Turn{
    problem = pb,
    phase = ReflectionPhase,
    participants =  player_names,
    biddings = player_bids,
    phaseTimer = t,
    activePlayer = ("",0)
  }


getGridFromFile :: String -> IO LevelStr
getGridFromFile filename = do
  let deleteNewLine s = replace "\n" "" s
  fmap deleteNewLine (readFile filename)


-- would be cool to remove quotes around names when sending the scoreboard
scoreboardToString :: Map ClientName Int -> String
scoreboardToString scoreboard = do
  let show_remove_quotes score = replace "\"" "" (show score)
  intercalate "" (map show_remove_quotes $ Map.toList scoreboard)
