module Phase
  (
  launchReflectionPhaseTimer,
  launchBiddingPhaseTimer,
  initiateResolutionPhase,
  stopPhaseTimer
  )
  where
import ServerData
import Communication
import TurnTools
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Timer -- handle game phase timers
import Control.Concurrent.Suspend.Lifted -- initiate Delay types
import Data.Time.Clock
import Data.Map (Map)
import qualified Data.Map as Map

-- weird type GHC.Int.Int64
timerTick = 1
reflection_duration = 300
bidding_duration = 30
resolve_duration = 60

-- set up a timer to change phase after 5 minutes
launchReflectionPhaseTimer :: Server -> IO Bool
launchReflectionPhaseTimer serv@Server{..} = do
  t <- readMVar currentTurn
  let phase_timer = phaseTimer t
  {-start_time <- getCurrentTime
  let phase_timer = phaseTimer turn
      run_timer = do
        current_time <- getCurrentTime
        let end_time = addUTCTime 300 start_time -- start time + 5 minutes
            time_val = round $ toRational $ diffUTCTime end_time current_time -- convert to int value
        broadcast serv $ Echo ("TIME TILL END OF THINKING PHASE : " ++ (show time_val))
  timmy <- newTimer
  repeatedStart timmy run_timer (sDelay timerTick) -}
--oneShotStart phase_timer (do stopTimer timmy; broadcast serv $ Echo "end reflection"; launchBiddingPhaseTimer serv; return ()) (sDelay reflection_duration)
  oneShotStart phase_timer (do broadcast serv $ Echo "TIME OUT FOR REFLECTION"; launchBiddingPhaseTimer serv; return ()) (sDelay reflection_duration)


launchBiddingPhaseTimer :: Server -> IO Bool
launchBiddingPhaseTimer serv@Server{..} = do
  turn <- takeMVar currentTurn
  broadcast serv $ Echo "BEGINNING BIDDING PHASE"
  {-start_time <- getCurrentTime
  let run_timer = do
      current_time <- getCurrentTime
      let end_time = addUTCTime 30 start_time -- start_time + 30s for bidding phase
          time_val = round $ toRational $ diffUTCTime end_time current_time -- convert to int value
      broadcast serv $ Echo ("TIME TILL END OF BIDDING PHASE : " ++ (show time_val))
  timmy <- newTimer
  repeatedStart timmy (run_timer) (sDelay timerTick)-}
  t <- newTimer
  putMVar currentTurn (turn {phaseTimer = t})
  --oneShotStart t (do stopTimer timmy; initiateResolutionPhase serv FirstAttempt; broadcast serv $ Echo "end bidding") (sDelay bidding_duration)
  oneShotStart t (do initiateResolutionPhase serv FirstAttempt; broadcast serv $ Echo "end bidding") (sDelay bidding_duration)



launchResolutionTimer :: Server -> IO Bool
launchResolutionTimer serv@Server{..} = do
  turn <- takeMVar currentTurn
  broadcast serv $ Echo "BEGINNING RESOLUTION PHASE"
  {-start_time <- getCurrentTime
  let run_timer = do
      current_time <- getCurrentTime
      let end_time = addUTCTime 60 start_time -- start_time + 60s for resolution
          time_val = round $ toRational $ diffUTCTime end_time current_time -- convert to int value
      broadcast serv $ Echo ("TIME TILL END OF RESOLUTION PHASE : " ++ (show time_val))
  broadcast serv $ Echo "before repeated start"
  timmy <- newTimer
  repeatedStart timmy run_timer (sDelay timerTick)-}
  t <- newTimer
  putMVar currentTurn (turn {phaseTimer = t})
--  oneShotStart t (do stopTimer timmy; initiateResolutionPhase serv PreviousTimeOut; broadcast serv $ Echo "end resolution") (sDelay resolve_duration)
  oneShotStart t (do initiateResolutionPhase serv PreviousTimeOut; broadcast serv $ Echo "end resolution") (sDelay resolve_duration)



initiateResolutionPhase :: Server -> ResolutionType -> IO ()
initiateResolutionPhase serv@Server{..} res_type = do
  -- DEBUT
  t <- readMVar currentTurn
  let next = getNextActivePlayerAndRemainingBids t
  case next of
    Nothing -> do -- bidding map was empty => no more players
      stopPhaseTimer serv
      broadcast serv $ EndResolutionPhase
    -- no winner so just set a new Turn
      turn <- initiateNewTurn serv
      s <- readMVar session
      broadcast serv $ SumUpTurn (problem turn) (scoreboardToString $ scoreboard s)
      launchReflectionPhaseTimer serv
      return ()
    Just ((name, bid), remaining_bids) -> do
      broadcast serv $ Echo ((show name) ++ " => " ++ (show bid))
      broadcast serv $ Echo ("remaining " ++ show remaining_bids)
      turn <- takeMVar currentTurn
      -- we do these updates in any case
      let update_bids = setTurnBids turn remaining_bids
          update_active_player = setActivePlayer update_bids (name, bid)
      case res_type of
        FirstAttempt -> do -- still in bidding phase so next player is the first one to resolve
          let new_turn = setTurnPhase update_active_player ResolutionPhase
          --stopTimer $ phaseTimer new_turn -- just in case
          putMVar currentTurn new_turn
          launchResolutionTimer serv
          --broadcast serv $ Echo "after launch"
          broadcast serv $ EndBiddingPhase name (show bid)
        PreviousTimeOut -> do
          putMVar currentTurn update_active_player
          launchResolutionTimer serv
          broadcast serv $ TimedOut name
        BadSolution -> do
          putMVar currentTurn update_active_player
          launchResolutionTimer serv
          broadcast serv $ SolutionRefused name


stopPhaseTimer :: Server -> IO()
stopPhaseTimer server@Server{..} = do
  t <- takeMVar currentTurn
  stopTimer $ phaseTimer t
  putMVar currentTurn t

getNextActivePlayerAndRemainingBids :: Turn -> Maybe ((ClientName, Int), Map ClientName Int)
getNextActivePlayerAndRemainingBids t = do
  -- got it wrong here.. should have made the bid board a Map (Int, ClientName)
  -- cannot search by value so we have to fiddle around
  let bids = biddings t
      elems = Map.elems bids -- no function to search by value..
      min_bid = minimum elems
      (min_map, remaining_bids) = Map.partition (== min_bid) bids -- should be unique
  if min_map == Map.empty then Nothing
  else do
      let min_tuple = head $ Map.toList min_map
      Just((min_tuple, remaining_bids))

setActivePlayer :: Turn -> (ClientName, Int) -> Turn
setActivePlayer t (name,bid) = t { activePlayer = (name,bid)}
