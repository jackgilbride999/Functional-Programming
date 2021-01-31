module AutoPlayer (
    autoPlayerMove
) where

import Board
import System.Random
import Data.Maybe


autoPlayerMove :: Board -> IO Board
autoPlayerMove board = 
    do
        let
            coordsList = [(rowIndices, columnIndices) | columnIndices <- [0 .. Board.width board - 1], rowIndices <- [0 .. Board.height board - 1]]
            firstTry =  evaluateRecursivePatten board flagPatternMatch coordsList 
            secondTry =  evaluateRecursivePatten board validUncoverPatternMatch coordsList 
            in
            if isJust firstTry
                then return $ fromMaybe board firstTry
                else if isJust secondTry
                    then return $ fromMaybe board secondTry
                    else do
                        randomMove board <$> newStdGen

-- uncover a completely random cell. If the cellStatus is not hidden, then try again              
randomMove :: Board -> StdGen -> Board 
randomMove board stdGen = 
        let
            (randomX, newGenerator) = randomR (0, width board-1) stdGen
            (randomY, newerGenerator) = randomR (0, height board-1) newGenerator
            cellStatus = getCellStatus board (randomX, randomY)
        in  
            if cellStatus == hidden 
            then expandCells (randomX, randomY) board
            else randomMove board newerGenerator

-- take a list, and evaluate a function that returns a Maybe Board on its first argument.
-- if the function result is a Just, return the Just. If it is a Nothing, continue the recursion.
evaluateRecursivePatten :: Board -> (Board -> (Int, Int) -> Maybe Board )->[(Int, Int)] -> Maybe Board
evaluateRecursivePatten _ _ [] = Nothing
evaluateRecursivePatten board f (xy : xys) = 
    let functionResult = f board xy in 
    if isJust functionResult 
    then functionResult
    else evaluateRecursivePatten board f xys

-- get the coordinates of all of the valid cells adjacent to (x, y)
getSurroundingSquares :: Board -> (Int, Int) -> [(Int, Int)]
getSurroundingSquares board (x, y) = 
    let unfiltered = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
    in filter (isInBoard board) unfiltered

-- take a list of cells, and update the first "hidden" one to "flagged"
flagAdjacentUnexposed :: Board -> [(Int, Int)] -> Board
flagAdjacentUnexposed board [] = board
flagAdjacentUnexposed board (xy : xys) = 
    if getCellStatus board xy == hidden
        then updateCellStatus xy board flagged
        else flagAdjacentUnexposed board xys

-- take a list of cells, and expand the first "hidden" one
expandFirstUnexposed :: Board -> [(Int, Int)] -> Board
expandFirstUnexposed board [] = board
expandFirstUnexposed board (xy : xys) = 
    if getCellStatus board xy == hidden
        then expandCells xy board
        else expandFirstUnexposed board xys

-- if the number of non-uncovered adjacent mines is equal to the cell number, they are all mines. If any are unflagged, flag ONE.
flagPatternMatch :: Board -> (Int, Int) -> Maybe Board
flagPatternMatch board (x, y) = 
    let 
        surroundingSquares = getSurroundingSquares board (x, y)
        numAdjacentUnexposed = length $ filter (== hidden) (map (getCellStatus board) surroundingSquares)
        numAdjacentFlags = length $ filter (== flagged ) (map (getCellStatus board) surroundingSquares)
        numAdjacentMines = getCellValue board (x, y)
    in if numAdjacentUnexposed > 0 && numAdjacentUnexposed + numAdjacentFlags == numAdjacentMines
        then Just $ flagAdjacentUnexposed board surroundingSquares
        else Nothing

-- if the cell value is equal to the number of flagged adjacent cells, then we have flagged all adjacent mines. If there are any hidden adjacent cells, uncover ONE.
validUncoverPatternMatch :: Board -> (Int, Int) -> Maybe Board
validUncoverPatternMatch board (x, y) =
    let 
        surroundingSquares = getSurroundingSquares board (x, y)
        numAdjacentUnexposed = length $ filter (== hidden) (map (getCellStatus board) surroundingSquares)
        numAdjacentFlags = length $ filter (== flagged ) (map (getCellStatus board) surroundingSquares)
        numAdjacentMines = getCellValue board (x, y)
    in 
        if getCellStatus board (x,y) == visible && numAdjacentFlags == numAdjacentMines && numAdjacentUnexposed > 0
        then Just $ expandFirstUnexposed board surroundingSquares
        else Nothing 