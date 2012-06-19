import Data.Array.IArray
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad

import System.Environment
import System.IO
import System.Exit

{- |The internal state of the Sudoku puzzle is represented as an array mapping
 - (row, column) pairs to a Maybe Int from 1-9 or Nothing. The array is
 - expected to be 9x9 -}
type SudokuState = Array (Int, Int) (Maybe Int)

validEntries :: [Int]
validEntries = [1..9]
validCoords = [0..8]

-- |Loads a sudoku puzzle from text; this simply picks out the first 81
-- digits in the given string and uses them to fill the array row-by-row
loadSudoku :: String -> SudokuState
loadSudoku str = array ((0,0), (8,8)) arrayAssocs
  where
    coords = [(row, col) | row <- validCoords, col <- validCoords]
    toMaybe digit = if digit == 0 then Nothing else Just digit
    rawInput = filter isDigit str
    arrayAssocs = zip coords $ map (toMaybe . read . return) $ rawInput

-- |Turns sudoku state into a nicely-formatted string for printing
prettySudoku :: SudokuState -> String
prettySudoku = intercalate "\n" . getLines . map toChar . elems
  where
    toChar (Just n) = head . show $ n
    toChar Nothing = '0'

    getLines [] = []
    getLines str = (take 9 str):(getLines . drop 9 $ str)

{- |Attempts to solve the given Sudoku puzzle using a backtracking algorithm.
 - Yields a final SudokuState in a Just on success, or Nothing on failure. -}
solveSudoku :: SudokuState -> Maybe SudokuState
solveSudoku state = if null empties 
                      then Just state
                      else join . find isJust . map solveSudoku $ nextStates
  where
    -- Coordinates of empty boxes
    empties :: [(Int, Int)]
    empties = map fst . filter (isNothing . snd) . assocs $ state
 
    -- To do: make this smarter
    target :: (Int, Int)
    target = let score = length . valids
               in minimumBy (\c1 c2 -> if score c1 < score c2 then LT
                 else if score c1 > score c2 then GT
                   else EQ) empties 

    -- Given a pair of coordinates referring to an empty box, yields which
    -- entries it may have based on what exists in its row, column, and 3x3
    -- square.
    valids :: (Int, Int) -> [Int]
    valids (row, col) = validRow `intersect` validCol `intersect` validSquare
      where
        validRow = validEntries \\ (catMaybes [state ! (row, c) | c <- validCoords])
        validCol = validEntries \\ (catMaybes [state ! (r, col) | r <- validCoords])
        validSquare = let squareCoords = [(r, c) | r <- [3*(row `div` 3)..3*(row `div` 3) + 2],
                                                   c <- [3*(col `div` 3)..3*(col `div` 3) + 2]]
                        in validEntries \\ (catMaybes . map (state !) $ squareCoords)

    -- A list of possible next states after filling in the target with each of
    -- its valid entries
    nextStates :: [SudokuState]
    nextStates = map (\valid -> state // [(target, Just valid)]) (valids target)

main = do
    args <- getArgs
    if null args then do
        hPutStrLn stderr "Error: you must specify a puzzle file"
        exitWith (ExitFailure 1)
        else return ()
    let filename = head args
    infile <- openFile filename ReadMode
    contents <- hGetContents infile
    let solution = solveSudoku . loadSudoku $ contents
    case solution of
        Just state -> putStrLn . prettySudoku $ state
        Nothing -> putStrLn "No solution"
    exitWith ExitSuccess
