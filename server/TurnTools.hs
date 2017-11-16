module TurnTools
(
  initiateNewTurn,
  setTurnPhase,
  addPlayerBid,
  setTurnBids,
)where

import ServerData
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent
import Control.Concurrent.MVar


-- maybe need to abort if can't get MVar on turn
initiateNewTurn :: Server -> IO Turn
initiateNewTurn serv@Server{..} = do
  s <- takeMVar session
  t <- tryTakeMVar currentTurn
  turn <- newTurn (level s) serv
  putMVar currentTurn turn
  let old_count = countTurn s
      update_session = s { countTurn = (old_count + 1)}
  putMVar session update_session
  return turn


setTurnPhase :: Turn -> Phase -> Turn
setTurnPhase t p = t {phase = p}

setTurnBids :: Turn -> Map ClientName Int -> Turn
setTurnBids t update_map = t { biddings = update_map }

addPlayerBid :: Turn -> ClientName -> Int -> Turn
addPlayerBid t name bid = do
  let biddingsmap = biddings t
      update = Map.insert name bid biddingsmap
  setTurnBids t update
