module Main (main) where

import Data.Array.IArray

import Data.List
import Data.Maybe
import Data.Char

import Control.Arrow

import System.Environment
import System.IO
import System.Exit

import Control.Monad
import Control.Parallel.Strategies

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
prettySudoku = intercalate "\n" . pad . chunk 9 . map toChar . elems
  where
    -- Maps each board entry (Maybe Int) to a character for display
    toChar (Just n) = head . show $ n
    toChar Nothing = '0'

    -- Breaks a list into sublists of fixed size
    chunk :: Int -> [a] -> [[a]]
    chunk _ [] = []
    chunk n xs = (take n xs):chunk n (drop n xs)

    -- Given n, x, and xs, inserts x after every nth entry in xs
    insertEveryNth :: Int -> a -> [a] -> [a]
    insertEveryNth n x = intercalate [x] . chunk n

    -- Adds whitespace between blocks to the line list
    pad :: [String] -> [String]
    pad = insertEveryNth 3 "" . map (insertEveryNth 3 ' ')

{- |Given a pair of coordinates referring to an empty box, yields which
 - entries it may have based on what exists in its row, column, and 3x3
 - square. -}
validEntries :: SudokuState -> (Int, Int) -> [Int]
validEntries state (row, col) = validRow `intersect` validCol `intersect`
                                    validSquare
  where
    -- Starting row/column for the 3x3 square in which (row, col) is contained
    (square_row, square_col) = (3*(row `div` 3), 3*(col `div` 3))

    validRow = arrayEntries \\ (catMaybes [state ! (row, c) |
        c <- arrayIndices, c /= col])

    validCol = arrayEntries \\ (catMaybes [state ! (r, col) |
        r <- arrayIndices, r /= row])

    validSquare = let square = [(r, c) | r <- [square_row..square_row + 2],
                                         c <- [square_col..square_col + 2],
                                         (r /= row) || (c /= col)]
                    in arrayEntries \\ (catMaybes $ map (state !) square)

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
    nextStates = map (\valid -> state // [(target, Just valid)])
        (validEntries state target)

{- |Validates the given puzzle state by checking if there are any conflicts along
 - a row, column, or in a 3x3 square -}
validateSudoku :: SudokuState -> Bool
validateSudoku state = and $
    -- (coord, entry) pairs for boxes which are already full
    let full = map (second fromJust) . filter (isJust . snd) . assocs $ state
      in map (\(coords, value) -> elem value $ validEntries state coords) full

main = do
    -- Read in the arguments, which are expected to be puzzle files
    args <- getArgs
    if null args then do
        hPutStrLn stderr "Error: you must specify at least one puzzle file"
        exitWith (ExitFailure 1)
        else return ()

    puzzles <- forM args $ \filename -> do
        infile <- openFile filename ReadMode
        contents <- hGetContents infile
        return . loadSudoku $ contents

    let solved = map solveSudoku puzzles `using` parTraversable rdeepseq
    let validated = map (fmap validateSudoku) solved `using` parTraversable rdeepseq
    let tagged = zip3 args solved validated

    sequence_ $ map (\(name, solution, valid) ->
        putStrLn name >> case solution of
            Just state -> if fromJust valid
                then putStrLn . prettySudoku $ state
                else hPutStrLn stderr "Solution did not validate"
            Nothing -> putStrLn "No solution") tagged

    exitWith ExitSuccess
