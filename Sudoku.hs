-- Sudoku.hs
-- Sudoku solver implemented using
-- constraint propagation and backtracking

import System.IO

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either

import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as M
import qualified Data.List as L

type Coord = (Int,Int)
type Board = [(Coord,Int)]
type Solution = [(Coord,Int)]
type Holes = [(Coord,[Int])]
type SudokuState = (Board, Holes, Solution)

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

row :: Board -> Coord -> Board
row b (_,r) = filter (\((_,y),_) -> y == r) b

col :: Board -> Coord -> Board
col b (c,_)  = filter (\((x,_),_) -> x == c) b

box :: Board -> Coord -> Board
box b (x,y) =
  filter (\((x',y'),_) -> xgood x' && ygood y') b
  where bx = (div (x-1) 3) + 1
        by = (div (y-1) 3) + 1
        xmin = 1 + ((bx-1)*3)
        xmax = xmin + 2
        ymin = 1 + ((by-1)*3)
        ymax = ymin + 2
        xgood x' = x' >= xmin && x' <= xmax
        ygood y' = y' >= ymin && y' <= ymax


-- get current values in row, col and box of cell
-- and filter these out as possible values for a cell
eliminatePossibleVals :: State SudokuState ()
eliminatePossibleVals = do
  (board, holes, sol) <- get

  -- eliminate possible values
  holes' <- forM holes $ \((x,y),vals) -> do
    let rowVals = map snd $ row board (x,y)
    let colVals = map snd $ col board (x,y)
    let boxVals = map snd $ box board (x,y)
    let badVals = rowVals ++ colVals ++ boxVals
    let vals' = filter (not . flip elem badVals) vals
    return ((x,y), vals')

  put (board, holes', sol)

-- if a hole has only one possible value,
-- add it to the solution
eliminateUnitaryHoles :: State SudokuState ()
eliminateUnitaryHoles = do
  (board, holes, sol) <- get
  let holes' = filter (\(_,vals) -> length vals > 1) holes
  let newSol = map (\(c,[x]) -> (c,x)) $ filter isUnitaryHole holes
  let sol' = foldr (:) sol newSol
  let board' = foldr (:) board newSol
  put (board', holes', sol')

  where isUnitaryHole (_, vals) = length vals == 1

-- aggressively propagate constraints
-- until we reach a steady state
propagateConstraints :: State SudokuState ()
propagateConstraints = do
  (_, holes, _) <- get
  eliminatePossibleVals
  eliminateUnitaryHoles
  (_, holes', _) <- get

  if holes == holes'
  then return ()
  else propagateConstraints
  

addSolution :: Coord -> Int ->  State SudokuState ()
addSolution (x,y) val = do
  (board, holes, sol) <- get
  let board' = ((x,y),val):board
  let sol' = ((x,y),val):sol
  put (board', holes, sol')

-- backtracking algorithm that finds all
-- possible solutions to a sudoku board
solveBoard :: Reader SudokuState (Maybe Solution)
solveBoard = do
  (board, holes, sol) <- ask :: Reader SudokuState SudokuState
  case holes of
    [] -> return (Just sol)
    otherwise -> do
      -- no possible solution if a hole has no possible value
      if any impossible holes
      then return Nothing
      else do
        -- pick hole with minimum possible vals
        -- this increases our chances of finding a solution faster
        let h@((x,y),vals) = L.minimumBy minPossibleVals holes
        let hs = L.delete h holes
        -- loop until solution is found
        rval' <- runEitherT $ forM vals $ \val -> do
          rval <- lift $ local (newState hs (x,y) val) solveBoard
          case rval of
            Just rsol -> left rsol
            Nothing   -> return ()

        case rval' of
          Left rsol'  -> return (Just rsol')
          Right _     -> return Nothing

  where minPossibleVals (_,v) (_,v') = compare (length v) (length v')
        impossible (_,[]) = True
        impossible _      = False
        newState h' (x,y) val = pc (x,y) val . updateHoles h'
        updateHoles h' (b,h,s) = (b,h',s)
        pc (x,y) v = execState (addSolution (x,y) v >> propagateConstraints)

-- convert a partially filled board to a sudoku state
-- by calculating the holes that need to be filled
boardToState :: Board -> SudokuState
boardToState b = (b, holes, [])
  where boardCells = map fst b
        allCells = [(x,y) | x <- [1..9], y <- [1..9]]
        notInBoard = filter (not . flip elem boardCells) allCells
        holes = map (\c -> (c, [1..9])) notInBoard

parseSudoku :: [[Int]] -> SudokuState
parseSudoku cb = execState propagateConstraints s
  where s = foldr parseCell ([],[],[]) allCells
        allCells = [(x,y) | x <- [0..8], y <- [0..8]]
        getCell (x,y) = ((cb !! y) !! x)
        isVal c = let v = getCell c in v >= 1 && v <= 9
        parseCell c@(x,y) (b,h,s)
          | isVal c   = (((x+1,y+1),getCell c):b, h, s)
          | otherwise = (b, ((x+1,y+1),[1..9]):h, s)

printBoard :: Board -> Bool -> IO ()
printBoard b sol = do
  putHLine
  forM_ [y | y <- [1..9]] $ \y -> do
    putStr "|"
    forM_ [x | x <- [1..9]] $ \x -> do
      case lookup (x,y) b of
        Just n -> do
          putStr (" " ++ show n ++ " ")
          drawVLine x
        Nothing -> do
          putStr (if sol then " . " else " ? ")
          drawVLine x

    putStrLn ""
    drawHLine y

  where putHLine = putStrLn "------------------------------------"
        drawHLine y = if y `mod` 3 == 0 then putHLine else return ()
        drawVLine x = if x `mod` 3 == 0 then putStr " | " else return ()


-- check if a board is valid
-- (i.e., no duplicate values for row/col/box)
validBoard :: Board -> Bool
validBoard b =
  let rowVals = map (map snd . row b) rowColCoords in
  let invalidRows = any hasDuplicates rowVals in
  let colVals = map (map snd . col b) rowColCoords in
  let invalidCols = any hasDuplicates colVals in
  let boxVals = map (map snd . box b) boxCoords in
  let invalidBoxes = any hasDuplicates boxVals in
  not $ invalidRows || invalidCols || invalidBoxes
  where rowColCoords = [(x,x) | x <- [1..9]]
        boxCoords = [((x*3)+1,(y*3)+1) | x <- [0..2], y <- [0..2]]

{-
possible solution:
board1 = [[4,5,2,3,9,1,8,7,6],
          [3,1,8,6,7,5,2,9,4],
          [6,7,9,4,2,8,3,1,5],
          [8,3,1,5,6,4,7,2,9],
          [2,4,5,9,8,7,1,6,3],
          [9,6,7,2,1,3,5,4,8],
          [7,9,6,8,5,2,4,3,1],
          [1,8,3,7,4,9,6,5,2],
          [5,2,4,1,3,6,9,8,7]]
-}
board1 = [[0,0,2,0,9,1,8,7,0],
          [3,1,0,0,7,0,0,9,0],
          [6,7,9,4,2,0,3,1,5],
          [8,0,1,0,6,4,7,2,9],
          [0,0,0,9,8,0,0,0,3],
          [0,0,0,2,0,3,5,4,8],
          [7,0,6,0,5,0,0,3,1],
          [0,8,3,0,4,0,0,5,2],
          [5,0,4,0,0,6,0,8,0]]

main = do
  hSetBuffering stdout NoBuffering
  let s@(board,_,_) = parseSudoku board1
  if not (validBoard board)
  then do
    putStrLn "input board is invalid (duplicate in row/col/box):"
    printBoard board False

  else do
    let sol = runReader solveBoard s
    case sol of
      Just sol' -> do
        putStrLn "input board: "
        printBoard board False

        putStrLn "solution: "
        printBoard sol' True
      Nothing -> do
        putStrLn "No solution possible for board: "
        printBoard board False
