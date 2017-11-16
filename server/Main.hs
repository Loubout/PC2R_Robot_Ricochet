module Main where

{-# LANGUAGE RecordWildCards #-}


import ServerData -- contains data types and such
import Communication -- handle transforming data into messages to players
import TurnTools
import Phase
import Level -- Matrix model of level etc..
import Solution -- tools to check solution received from players
import ConcurrentUtils
import Network
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Async
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Text.Printf
import Data.List.Split(splitOn) -- split messages on '/'
import Data.List
import DataStringUtils (replace) -- remove \r in message received form network


port :: Int
port = 2016

main :: IO()
main = withSocketsDo $ do
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle server) (\_ -> hClose handle) -- give each new client a thread and a handle


talk :: Handle -> Server -> IO ()
talk h server = do
  hSetBuffering h LineBuffering
  connectLoop
  where
    connectLoop = do
      line <- hGetLine h
      putStrLn $ "Received : " ++ line
      if null line then connectLoop
        else do
          let cmd = splitOn "/" line
          case cmd of
            ["CONNEXION", name] -> do
              -- THOSE DISGUSTING \r ruining string concatenation and Map.member
              check <- checkAddNewPlayer server (replace "\r" "" name) h
              case check of
                Nothing -> do
                  hPutStrLn h "Error : name already taken"
                  connectLoop
                Just client -> do
                  initializeNewPlayer server client
                  runClient server client
            _ -> hPutStrLn h "Error : message not relevant to protocol"


setupNewSession :: Server -> IO ()
setupNewSession server@Server{..} = do
  start <- attemptNewSession server
  case start of
    Just new_session -> do -- need to send the problem to everyone
      let scores = scoreboard new_session
          nb_turn = countTurn new_session
          report_string = (show nb_turn) ++ (scoreboardToString scores)
      turn <- initiateNewTurn server
      launchReflectionPhaseTimer server
      -- send the relevant info while waiting for the next turn
      broadcast server $ SumUpTurn (problem turn) report_string
    _ -> broadcast server $ Echo "waiting for more players"


initializeNewPlayer :: Server -> Client -> IO()
initializeNewPlayer server@Server{..} client@Client{..} = do
  sendMessage client $ Welcome $ cName
  ongoing_session <- isThereSession server-- tests wether there's a session already
  case ongoing_session of
    Nothing -> setupNewSession server
    Just session -> do
      turn <- readMVar currentTurn
      let scores = scoreboard session
          nb_turn = countTurn session
          report_string = (show nb_turn) ++ (scoreboardToString scores)
      sendMessage client $ OpenSession (level session)
      sendMessage client $ SumUpTurn (problem turn) report_string
      sendMessage client $ Echo "A game is currently in progress.. please wait for the next turn"


-- main loop running for each client
runClient :: Server -> Client -> IO()
runClient serv@Server{..} client@Client{..} = do
  -- launch a thread that simply transfer broadcast messages to the client cf Communication.hs
  forkFinally (monitorBroadcast cBroadcastChan cSendChannel) (\_ -> hClose cHandle)
  -- each client has one thread to receive from the network and one to send messages
  race server receive --
  return ()
 where
  receive = forever $ do
    msg <- hGetLine cHandle -- read from the network
    -- DEBUG DEBUG
    putStrLn $ "Received : " ++ msg
    let cmd = splitOn "/" msg
    case cmd of
      ["SORT", name] -> do
        removeClient serv name -- todo = need to remove player from session and turn and notify
      ["DIREATOUS", txt] -> do
        broadcast serv $ TellEveryone cName txt -- basic chat
      ["TROUVE", name, nb_moves] -> do
        if name /= cName then do
          sendMessage client $ Echo "Error .. username and client do not match"
        else do
          cur_turn <- takeMVar currentTurn -- lock is taken
          let val_nb_moves = read nb_moves :: Int
              check_proposal = validateFirstProposal cur_turn name val_nb_moves
          case check_proposal of
            False -> do   -- put back the same turn and sned message to bad player
              putMVar currentTurn cur_turn
              sendMessage client $ Echo "Proposed solution is not valid : invalid number of moves or invalid participant"
            True -> do
              let turn_first_bid = addPlayerBid cur_turn name val_nb_moves -- update turn with new bid
                  updated_turn = setTurnPhase turn_first_bid BiddingPhase  -- change of game phase
              --stopReflectionPhaseTimer serv  -- BUG CUZ MVAR ALREADY TAKEN ABOVE  + kinda useless since there's only one timer
              putMVar currentTurn updated_turn -- Turn MVar is updated
              launchBiddingPhaseTimer serv  -- this will stop the reflection timer anyway
              sendMessage client $ ValidateSolution
              broadcast serv $ FirstSolutionNotification name nb_moves
      ["ENCHERE", name, nb_moves] -> do
        if name /= cName then do
          sendMessage client $ Echo "Error .. username and client do not match"
        else do
          cur_turn <- takeMVar currentTurn
          let val_nb_moves = read nb_moves :: Int
              check_bid = validatePlayerBid cur_turn name val_nb_moves
          case check_bid of
            InvalidParticipant -> do
              putMVar currentTurn cur_turn -- put back turn state in place and release lock
              sendMessage client $ Echo "Can't do that bid : invalid player"
            InvalidNbMoves -> do
              putMVar currentTurn cur_turn -- put back turn state in place and release lock
              sendMessage client $ Echo "Can't do that bid : number of moves < 0 "
            BiddingPhaseOver -> do
              putMVar currentTurn cur_turn -- put back turn state in place and release lock
              sendMessage client $ Echo "Can't do that bid : bidding phase is over "
            IncoherentBid user_name -> do
              --sendMessage client $ Echo $ "DEBUG .. biddings :" ++ (show (Map.toList (biddings cur_turn)))
              putMVar currentTurn cur_turn -- put back turn state in place and release lock
              sendMessage client $ FailureBid user_name
            OkBid -> do
              let updated_turn = addPlayerBid cur_turn name val_nb_moves
              sendMessage client $ ValidateBid
              broadcast serv $ BidNotification name nb_moves
              putMVar currentTurn updated_turn
      ["SOLUTION", name, str_moves] -> do

        if name /= cName then sendMessage client $ Echo "Error .. username and client do not match"
        else do
          s <- readMVar session
          t <- readMVar currentTurn
          let (active_name, active_bid) = activePlayer t
              moves = parsePlayerSolution str_moves
              nb_moves = length moves -- to check that the player doesn't go over his bid
          if cName /= active_name then sendMessage client $ Echo "Error .. user is not the active player"
          else if nb_moves > active_bid then
             do broadcast serv $ Echo ("TOO MANY MOVES BRO.. U SAID " ++ show active_bid ++ " not " ++ show nb_moves);
                initiateResolutionPhase serv BadSolution
              else do
              let pb = problem t
                  grid = getGridFromString (level s)
                  mat = buildProblemGrid grid
                  check_solution = verifyPlayerSolution pb moves mat
              broadcast serv $ ProposedSolutionNotification name moves
              case check_solution of
                True -> do
                  broadcast serv $ SolutionAccepted -- player has it right
                  updatePlayerScore serv name -- this will announce victory
                  stopPhaseTimer serv
                  turn <- initiateNewTurn serv
                  s <- readMVar session
                  let nb_turn = countTurn s
                      scores = scoreboard s
                      report_string = (show nb_turn) ++ (scoreboardToString scores)
                  broadcast serv $ SumUpTurn (problem turn) report_string
                False -> do
                  initiateResolutionPhase serv BadSolution -- this shoud take care of everything
      _ -> sendMessage client $ Echo $ "Error.. echo from server : " ++ msg

  server = join $ do
    chan <- readMVar cSendChannel
    msg <- readChan chan -- read from channel
    return $ do
      continue <- handleSendingMessage serv client msg -- handleMessage returns boolean
      when continue $ server


-- compare incr

updatePlayerScore :: Server -> ClientName -> IO ()
updatePlayerScore serv@Server{..} name = do
  s <- takeMVar session
  let scores = scoreboard s
      old_score = Map.findWithDefault 0 name scores -- could use a pretty insertWith but no time
      new_score = old_score + 1
      nb_turn = countTurn s
      update_scores = Map.insert name new_score scores
      new_session = s {scoreboard = update_scores}
      report_string = (show nb_turn) ++ (scoreboardToString update_scores)
  putMVar session new_session
  if new_score == objective then do
    broadcast serv $ AnnounceWinner report_string
    stopPhaseTimer serv
    setupNewSession serv
    broadcast serv $ Echo "Session has been won.."
  else return ()



-- check if the player is amongst the participants of the turn and if nb_moves is a positive integer
validateFirstProposal :: Turn -> ClientName -> Int -> Bool
validateFirstProposal turn name nb_moves = do
  let names = participants turn
  if phase turn == ReflectionPhase && elem name names && nb_moves > 0 then True
  else False

validatePlayerBid :: Turn -> ClientName -> Int -> BidOutcome
validatePlayerBid turn name nb_moves = do
  let names = participants turn
      biddingsmap = biddings turn -- get a list of the current bids
      check_phase = phase turn == BiddingPhase
      check_participant = elem name names
      check_value = nb_moves > 0
      check_previous_own = nb_moves < Map.findWithDefault maxBound name biddingsmap
      check_other_bids = notElem nb_moves $ Map.elems biddingsmap

  if not check_phase then BiddingPhaseOver
    else if not check_participant then InvalidParticipant
        else if not check_value then InvalidNbMoves
            else if not check_previous_own then IncoherentBid name
                else if not check_other_bids then do
                  let list_bids = Map.toList biddingsmap
                      search_name ((n, nb_moves):xs) = n
                      search_name (_:xs) = search_name xs
                  IncoherentBid $ search_name list_bids
                  --NEED TO INSERT CANNOT BID IF OTHER PLAYER HAS SAME VALUE
                else OkBid


-- Returns nothing if there a no current session and (Just session) otherwise
isThereSession :: Server -> IO (Maybe Session)
isThereSession server@Server{..} = do
  cur_session <- tryTakeMVar session
  case cur_session of
    Nothing -> return Nothing
    (Just s) -> do
      putMVar session s
      return $ Just s


attemptNewSession :: Server -> IO (Maybe Session)
attemptNewSession server@Server{..} = do
  clientmap <- readMVar clients
  if (Map.size clientmap > 1) then do
    s <- tryTakeMVar session -- session is either empty or we are erasing the previous one
    new_session <- newSession server
    putMVar session new_session -- no need to take because session is empty
    broadcast server $ OpenSession (level new_session)
    return $ Just new_session
  else return Nothing



-- attempts to add a new player if the chosen name is note already taken
checkAddNewPlayer :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddNewPlayer server@Server{..} name handle = do
  clientmap <- readMVar clients -- retrieve all clients from server data Type = Other
  if Map.member name clientmap
    then return Nothing -- name alerady exists
    else do
      client <- newClient name broadcastChan handle
      old_clients <- takeMVar clients
      putMVar clients $ Map.insert name client clientmap
      broadcast server $ ConnectNotification name
      return (Just client)


removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = do
  clientmap <- takeMVar clients
  let update_clients = Map.delete name clientmap
  putMVar clients $ update_clients
  --if Map.size update_clients < 3 then do -- not enough player anymore ..
  -- but let's the poor lonely guy play on his own
  -- remove from scoreboard and or maybe make score persistent through a session ??
  broadcast server $ DisconnectNotification name
