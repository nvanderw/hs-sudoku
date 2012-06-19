import Data.Array.IArray

import Data.List
import Data.Maybe
import Data.Char

import Control.Monad
import Control.Arrow

import System.Environment
import System.IO
import System.Exit

{- |The internal state of the Sudoku puzzle is represented as an array mapping
 - (row, column) pairs to a Maybe Int from 1-9 or Nothing. The array is
 - expected to be 9x9 -}
type SudokuState = Array (Int, Int) (Maybe Int)

arrayEntries = [1..9]
arrayIndices = [0..8]

-- |Loads a sudoku puzzle from text; this simply picks out the first 81
-- digits in the given string and uses them to fill the array row-by-row
loadSudoku :: String -> SudokuState
loadSudoku str = array ((0,0), (8,8)) arrayAssocs
  where
    coords = [(row, col) | row <- arrayIndices, col <- arrayIndices]
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

{- |Given a pair of coordinates referring to an empty box, yields which
 - entries it may have based on what exists in its row, column, and 3x3
 - square. -}
validEntries :: SudokuState -> (Int, Int) -> [Int]
validEntries state (row, col) = validRow `intersect` validCol `intersect`
                                    validSquare
  where
    validRow = arrayEntries \\ (catMaybes [state ! (row, c) | c <- arrayIndices, c /= col])
    validCol = arrayEntries \\ (catMaybes [state ! (r, col) | r <- arrayIndices, r /= row])
    validSquare = let squareCoords = [(r, c) | r <- [3*(row `div` 3)..3*(row `div` 3) + 2],
                                               c <- [3*(col `div` 3)..3*(col `div` 3) + 2],
                                               (r /= row) || (c /= col)]
                    in arrayEntries \\ (catMaybes . map (state !) $ squareCoords)

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
 
    -- Next box to fill in
    target :: (Int, Int)
    target = head empties 

    -- A list of possible next states after filling in the target with each of
    -- its valid entries
    nextStates :: [SudokuState]
    nextStates = map (\valid -> state // [(target, Just valid)]) (validEntries state target)

{- |Validates the given puzzle state by checking if there are any conflicts along
 - a row, column, or in a 3x3 square -}
validateSudoku :: SudokuState -> Bool
validateSudoku state = and $
    let fullEntries = map (second fromJust) . filter (isJust . snd) . assocs $ state
      in map (\(coords, value) -> elem value . validEntries state $ coords) fullEntries

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
        Just state -> if validateSudoku state
                        then putStrLn . prettySudoku $ state
                        else hPutStrLn stderr "Solution did not validate"
        Nothing -> putStrLn "No solution"
    exitWith ExitSuccess
