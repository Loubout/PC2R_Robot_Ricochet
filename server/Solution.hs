module Solution
(
parsePlayerSolution,
verifyPlayerSolution
)where

import ServerData
import Level
import Data.Matrix
import Data.List
import Text.Regex.Posix
import Data.List.Split(splitOn)
import Control.Monad

{- This module contains the algorithm that allows if a solution sent by a player is a correct one -}

data Color = Red | Blue | Green | Yellow


-- need to make some getters for the Problem Data type which is a big tuple
-- did not want to use lists...
getRobotPos :: Color -> Problem -> (Int,Int)
getRobotPos Red ((x,y), (_,_), (_,_), (_,_), (_,_,_)) = (x,y)
getRobotPos Blue ((_,_), (x,y), (_,_), (_,_), (_,_,_)) = (x,y)
getRobotPos Yellow ((_,_), (_,_), (x,y), (_,_), (_,_,_)) = (x,y)
getRobotPos Green ((_,_), (_,_), (_,_), (x,y), (_,_,_)) = (x,y)
getTargetPos :: Problem -> (Int,Int)
getTargetPos   ((_,_), (_,_), (_,_), (_,_), (xc,yc, _)) = (xc,yc)
getTargetColor :: Problem -> Color
getTargetColor ((_,_), (_,_), (_,_), (_,_), (_,_,'R')) = Red
getTargetColor ((_,_), (_,_), (_,_), (_,_), (_,_,'B')) = Blue
getTargetColor ((_,_), (_,_), (_,_), (_,_), (_,_,'J')) = Yellow
getTargetColor ((_,_), (_,_), (_,_), (_,_), (_,_,'V')) = Green
getTargetTuple :: Problem -> (Int,Int,Char)
getTargetTuple ((_,_), (_,_), (_,_), (_,_), (x,y,c)) = (x,y,c)


data Direction = ToTheTop | ToTheRight | ToTheBottom | ToTheLeft




getMoveDir :: SingleMove -> Direction
getMoveDir move = do
  let d = snd move
  case d of
    'H' -> ToTheTop
    'B' -> ToTheBottom
    'G' -> ToTheLeft
    'D' -> ToTheRight

getMoveColor :: SingleMove -> Color
getMoveColor move = do
  let c = fst move
  case c of
    'R' -> Red
    'B' -> Blue
    'J' -> Yellow
    'V' -> Green


-- build list of tuple for each move from string received from player
parsePlayerSolution :: String -> SolutionMoves
parsePlayerSolution s = do
  let moves_str = getAllTextMatches $ s =~ "(R|B|J|V)(H|B|G|D)" :: [String]
      tuplify (r:dir) = (r,head dir)
  fmap tuplify moves_str


-- returns true if the player solution is correct
-- go recursively through each move and calculate the change of positions
-- when there's no more move to process compare the position of the target & robot
verifyPlayerSolution ::  Problem -> SolutionMoves -> Matrix GridCell -> Bool
verifyPlayerSolution pb [] mat = do
  let t_pos = getTargetPos pb
      t_color = getTargetColor pb
  --putStrLn $ show t_pos
  --putStrLn $ show (getRobotPos t_color pb)
  if t_pos == (getRobotPos t_color pb) then True
  else False

verifyPlayerSolution pb (m:remaining_moves) mat = do
  let red_pos = getRobotPos Red pb
      blue_pos = getRobotPos Blue pb
      yellow_pos = getRobotPos Yellow pb
      green_pos = getRobotPos Green pb
      target = getTargetTuple pb
      move_color = getMoveColor m
      move_dir = getMoveDir m
  case move_color of
    Red -> do
      let new_pos = lookForNextPosition red_pos move_dir ([blue_pos]++[yellow_pos]++[green_pos]) mat
      verifyPlayerSolution (new_pos, blue_pos, yellow_pos, green_pos, target) remaining_moves mat
    Blue -> do
      let new_pos = lookForNextPosition blue_pos move_dir ([red_pos]++[yellow_pos]++[green_pos]) mat
      verifyPlayerSolution (red_pos, new_pos, yellow_pos, green_pos, target) remaining_moves mat
    Yellow -> do
      let new_pos = lookForNextPosition yellow_pos move_dir ([red_pos]++[blue_pos]++[green_pos]) mat
      verifyPlayerSolution (red_pos, blue_pos, new_pos, green_pos, target) remaining_moves mat
    Green -> do
      let new_pos = lookForNextPosition green_pos move_dir ([red_pos]++[blue_pos]++[yellow_pos]) mat
      verifyPlayerSolution (red_pos, blue_pos, yellow_pos, new_pos, target) remaining_moves mat


-- some checkers for the gridCell tuple
wallAbove (True,_,_,_) = True
wallAbove (False,_,_,_) = False

wallAtTheRight (_,True,_,_) = True
wallAtTheRight (_,False,_,_) = False

wallBelow (_,_,True,_) = True
wallBelow (_,_,False,_) = False

wallAtTheLeft (_,_,_,True) = True
wallAtTheLeft (_,_,_,False) = False

-- maye can replace with
----------------------------------------


-- from a starting position, a direction, positions of other robots and a matrix
lookForNextPosition :: (Int,Int) -> Direction -> [(Int, Int)] -> Matrix GridCell -> (Int, Int)
lookForNextPosition (x,y) ToTheTop robots_pos mat = do
  let cur_pos = safeGet x y mat
  case cur_pos of
    Nothing -> (0, y) -- out of bound too far to the top
    Just (t,r,b,l) -> do
      if wallAbove (t,r,b,l) || elem (x-1,y) robots_pos then (x,y)
      else lookForNextPosition (x-1,y) ToTheTop robots_pos mat

lookForNextPosition (x,y) ToTheRight robots_pos mat = do
  let cur_pos = safeGet x y mat
  case cur_pos of
    Nothing -> (x, 16) -- out of bound too far to the right
    Just (t,r,b,l) -> do
      if wallAtTheRight (t,r,b,l) || elem (x,y+1) robots_pos then (x,y)
      else lookForNextPosition (x,y+1) ToTheRight robots_pos mat

lookForNextPosition (x,y) ToTheBottom robots_pos mat = do
  let cur_pos = safeGet x y mat
  case cur_pos of
    Nothing -> (16, y) -- out of bound too far to the bottom
    Just (t,r,b,l) -> do
      if wallBelow (t,r,b,l) || elem (x+1,y) robots_pos then (x,y)
      else lookForNextPosition (x+1,y) ToTheBottom robots_pos mat

lookForNextPosition (x,y) ToTheLeft robots_pos mat = do
  let cur_pos = safeGet x y mat
  case cur_pos of
    Nothing -> (x, 0) -- out of bound too far to the left
    Just (t,r,b,l) -> do
       if wallAtTheLeft (t,r,b,l) || elem (x,y-1) robots_pos then (x,y)
       else lookForNextPosition (x,y-1) ToTheLeft robots_pos mat
