module Level(
  GridCell,
  getGridFromString,
  buildProblemGrid,
  generateRandomProblem
)where

import Data.Matrix
import Data.List
import System.Random
import Text.Regex.Posix
import Data.List.Split(splitOn)


grid_size :: Int
grid_size = 16


-- CSS LIKE   -- TOP, RIGHT, BOTTOM, LEFT
type GridCell = (Bool, Bool, Bool, Bool) -- indicate the walls for each cell
-- some getters
top    (x,_,_,_) = x
right  (_,x,_,_) = x
bottom (_,_,x,_) = x
left   (_,_,_,x) = x

-- and setters
putTopWall    (t,r,b,l) = (True,r,b,l)
putRightWall  (t,r,b,l) = (t,True,b,l)
putBottomWall (t,r,b,l) = (t,r,True,l)
putLeftWall   (t,r,b,l) = (t,r,b,True)

-- [(Int,Int,Char)] is GameLevel type but don't wanna create duplicate definition
-- or import cycle

-- TEST MAYBE NEED TO APPLY +1 HERE SINCE FILE IS WRITTEN WITH INDEX START AT 0
getGridFromString :: String -> [(Int,Int,Char)]
getGridFromString lvl = do
  let wall_str = getAllTextMatches $ lvl =~ "[0-9]+,[0-9]+,(H|D|B|G)" :: [String]
      split s = splitOn "," s
      walls_list = fmap split wall_str
      tuplify [x,y,dir] = (1 + read x :: Int, 1 + read y :: Int, head dir)
      walls = fmap tuplify walls_list
  walls


buildProblemGrid :: [(Int,Int,Char)] -> Matrix GridCell
buildProblemGrid level = do
  let gen (x, y) = (False, False, False, False) -- create matrix with no walls
      empty_mat = matrix 16 16 gen
      -- added a function to put walls on all the borders
      walled_mat = updateBorders empty_mat
  updateWalls level walled_mat


updateBorders :: Matrix GridCell -> Matrix GridCell
updateBorders mat = do
  let mat_size = 16 :: Int
  let update_left_border 0 mat = mat
      update_left_border x mat = do
        let cell = getElem x 1 mat
        update_left_border (x-1) (setElem (putLeftWall cell) (x, 1) mat)
  let update_right_border 0 mat = mat
      update_right_border x mat = do
        let cell = getElem x mat_size mat
        update_right_border (x-1) (setElem (putRightWall cell) (x, mat_size) mat)
  let update_top_border 0 mat = mat
      update_top_border x mat = do
        let cell = getElem 1 x mat
        update_top_border (x-1) (setElem (putTopWall cell) (1, x) mat)
  let update_bottom_border 0 mat = mat
      update_bottom_border x mat = do
        let cell = getElem mat_size x mat
        update_bottom_border (x-1) (setElem (putBottomWall cell) (mat_size, x) mat)
  update_top_border mat_size (update_right_border mat_size (update_bottom_border mat_size (update_left_border mat_size mat)))



-- runs through the list of wall (x,y,side) and update the matri recursively
updateWalls :: [(Int,Int,Char)] -> Matrix GridCell ->  Matrix GridCell
updateWalls [] mat =  mat
updateWalls (h:s) mat = do
  -- for each wall item, update the corresponding cell AND its neighbour
  -- which thus has a wall on the opposite side
  let update w mat = do
      case w of
        (x, y, 'H') -> do
          let cur_val = getElem (x) (y) mat
              new_val = putTopWall cur_val
              up_fst_cell = setElem new_val (x, y) mat
              neigbour = safeGet (x-1) (y) up_fst_cell
          case neigbour of
            Nothing -> up_fst_cell -- safeGet return nothing if out of bound coordinates
            (Just val) -> do -- get current elem and add a bottom wall ...
               let new_cell = putBottomWall val
               setElem new_cell (x-1, y) up_fst_cell
        (x, y, 'D') -> do
          let cur_val = getElem (x) (y) mat
              new_val = putRightWall cur_val
              up_fst_cell = setElem new_val (x, y) mat
              neigbour = safeGet (x) (y+1) up_fst_cell -- right cell
          case neigbour of
            Nothing -> up_fst_cell
            (Just val) -> do
               let new_cell = putLeftWall val
               setElem new_cell (x,y+1) up_fst_cell
        (x, y, 'B') -> do
          let cur_val = getElem (x) (y) mat
              new_val = putBottomWall cur_val
              up_fst_cell = setElem new_val (x, y) mat
              neigbour = safeGet (x+1) (y) up_fst_cell -- below cell
          case neigbour of
            Nothing -> up_fst_cell
            (Just val) -> do
               let new_cell = putTopWall val
               setElem new_cell (x+1,y) up_fst_cell
        (x, y, 'G') -> do
          let cur_val = getElem (x) (y) mat
              new_val = putLeftWall cur_val
              up_fst_cell = setElem new_val (x, y) mat
              neigbour = safeGet (x) (y-1) up_fst_cell -- cell at the left
          case neigbour of
            Nothing -> up_fst_cell
            (Just val) -> do
               let new_cell = putRightWall val
               setElem new_cell (x,y-1) up_fst_cell
  updateWalls s (update h mat)

-- FOR LEVEL RANDOMIZATION

-- convert to list to make it easier to count 'True' elements ie walls
-- walls go TOP, RIGHT,BOTTOM,LEFT so =>
hasContiguousWalls :: GridCell -> Bool
hasContiguousWalls (True,_,_,True) = True
hasContiguousWalls (True,True,_,_) = True
hasContiguousWalls (_,True,True,_) = True
hasContiguousWalls (_,_,True,True) = True
hasContiguousWalls (_,_,_,_) = False -- in any other case

-- pick random coordinates until we find a suitable cell ie that has two constiguous walls
findRandomTargetPos :: Matrix GridCell -> IO (Int, Int)
findRandomTargetPos mat = do
  (x, y) <- getRandomCoordinates
  let (t,r,b,l) = getElem x y mat
  if hasContiguousWalls (t,r,b,l) then return (x,y)
  else findRandomTargetPos mat -- hopefully there will be no infinite loop



getRandomTarget :: Matrix GridCell -> IO (Int,Int,Char)
getRandomTarget mat = do
  index <- randomRIO(0,3)
  let c = ['R', 'B', 'J', 'V'] !! index -- pick a random color
  (x,y) <- findRandomTargetPos mat
  return (x,y,c)


-- provides random coordinates for
getRandomCoordinates :: IO (Int, Int)
getRandomCoordinates = do
  x <- randomRIO (1, grid_size)
  y <- randomRIO (1, grid_size)
  return (x, y)

-- provides four couples of random coordinates
generateRandomProblem :: String -> IO ((Int,Int), (Int,Int), (Int,Int), (Int,Int), (Int,Int,Char))
generateRandomProblem lvl_str = do
  let lvl = getGridFromString lvl_str
      mat = buildProblemGrid lvl
  red <- getRandomCoordinates
  blue <- getRandomCoordinates
  yellow <- getRandomCoordinates
  green <- getRandomCoordinates
  target <- getRandomTarget mat
  return (red, blue, yellow, green, target)
