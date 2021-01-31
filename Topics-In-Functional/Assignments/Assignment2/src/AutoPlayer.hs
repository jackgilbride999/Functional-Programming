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
            firstTry =  evaluateRecursivePatten board basicPattern coordsList 
            secondTry =  evaluateRecursivePatten board basicPattern2 coordsList 
            in
            if isJust firstTry
                then return $ fromMaybe board firstTry
                else if isJust secondTry
                    then return $ fromMaybe board secondTry
                    else do
                        randomMove board <$> newStdGen
                
evaluateRecursivePatten :: Board -> (Board -> (Int, Int) -> Maybe Board )->[(Int, Int)] -> Maybe Board
evaluateRecursivePatten _ _ [] = Nothing
evaluateRecursivePatten board f (xy : xys) = 
    let calculation = f board xy in 
    if isJust calculation 
    then calculation
    else evaluateRecursivePatten board f xys



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

getSurroundingSquares :: Board -> (Int, Int) -> [(Int, Int)]
getSurroundingSquares board (x, y) = 
    let unfiltered = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
    in filter (isInBoard board) unfiltered

flagAdjacentUnexposed :: Board -> [(Int, Int)] -> Board
flagAdjacentUnexposed board [] = board
flagAdjacentUnexposed board (xy : xys) = 
    if getCellStatus board xy == hidden
        then updateCellStatus xy board flagged
        else flagAdjacentUnexposed board xys

basicPattern :: Board -> (Int, Int) -> Maybe Board
basicPattern board (x, y) = 
    let 
        surroundingSquares = getSurroundingSquares board (x, y)
        numAdjacentUnexposed = length $ filter (== hidden) (map (getCellStatus board) surroundingSquares)
        numAdjacentFlags = length $ filter (== flagged ) (map (getCellStatus board) surroundingSquares)
        numAdjacentMines = getCellValue board (x, y)
    in if numAdjacentUnexposed > 0 && numAdjacentUnexposed + numAdjacentFlags == numAdjacentMines
        then Just $ flagAdjacentUnexposed board surroundingSquares
        else Nothing

basicPattern2 :: Board -> (Int, Int) -> Maybe Board
basicPattern2 board (x, y) =
    let 
        surroundingSquares = getSurroundingSquares board (x, y)
        numAdjacentExposed = length $ filter (== visible) (map (getCellStatus board) surroundingSquares)
        numAdjacentFlags = length $ filter (== flagged ) (map (getCellStatus board) surroundingSquares)
    in 
        if getCellStatus board (x,y) == hidden && numAdjacentExposed + numAdjacentFlags == length surroundingSquares
        then Just $ expandCells (x, y) board
        else Nothing 