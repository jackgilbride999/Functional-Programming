module AutoPlayer (
    autoPlayerMove
) where

import Board
import System.Random


autoPlayerMove :: Board -> IO Board
autoPlayerMove board = 
    do
        stdGen <- newStdGen 
        return $ randomMove board stdGen

randomMove :: Board -> StdGen -> Board 
randomMove board stdGen = 
        let
            (randomX, newGenerator) = randomR (0, width board-1) stdGen
            (randomY, newerGenerator) = randomR (0, height board-1) newGenerator
            cellStatus = getCellStatus (randomX, randomY) board
        in  
            if cellStatus == hidden 
            then updateCellStatus (randomX, randomY) board visible
            else randomMove board newerGenerator