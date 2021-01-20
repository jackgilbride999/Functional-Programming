module Board (
    Board,
    getCellValue, 
    initializeBoard
) where

import Data.Vector
import System.Random

type Proximity = Int

data Board = Board {
    cells :: Vector (Vector Proximity),
    width :: Int,
    height :: Int
}

mine = -1 :: Proximity
blank = 0 :: Proximity

createEmptyBoard :: Int -> Int -> Board
createEmptyBoard width height = Board (generate width (\_ -> Data.Vector.replicate height mine)) width height

-- todo add checks to see if outside of board
getCellValue :: (Int, Int) -> Board -> Proximity
getCellValue (x,y) board = (cells board) ! x ! y

updateCellValue :: (Int, Int) -> Board -> Proximity -> Board
updateCellValue (x, y) board value = 
    let 
        newRows = update ((cells board) ! x) (singleton (y, value)) -- update row x with the value
        newCells = update (cells board) (singleton (x, newRows))    -- update the cells with the new row x
    in Board newCells (width board) (height board)                  -- update the board with the new cells

incrementCellValue :: (Int, Int) -> Board -> Board
incrementCellValue coords board = updateCellValue coords board $ (getCellValue coords board) + 1

updateAdjacentCells :: (Int, Int) -> Board -> Board
updateAdjacentCells (x,y) board = incrementCells [(x-1, y-1), (x,y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x,y+1), (x+1, y+1)] board
 where 
     incrementCells [] board = board
     incrementCells (head:tail) board = 
         if getCellValue head board == mine 
         then
            incrementCells tail board
         else
            incrementCellValue head $ incrementCells tail board

plantMine :: (Int, Int) -> Board -> Board
plantMine coords board = 
    let boardWithMinePlanted = updateCellValue coords board mine -- plant a mine at the coords
    in updateAdjacentCells coords boardWithMinePlanted           -- update adjecent cells

populateBoard :: Board -> Int -> StdGen -> Board
populateBoard board 0 _ = board
populateBoard board numMines generator =
    let 
        (randomX, newGenerator) = randomR (0, (width board)-1) generator
        (randomY, newerGenerator) = randomR (0, (height board)-1) newGenerator
        in
            if getCellValue (randomX, randomY) board == mine
                then populateBoard board numMines newerGenerator
                else 
                    let plantedBoard = plantMine (randomX, randomY) board  
                    in populateBoard plantedBoard (numMines-1) newerGenerator

initializeBoard :: Int -> Int -> Int -> StdGen -> Board
initializeBoard width height numMines generator = 
    let emptyBoard = createEmptyBoard width height
    in populateBoard emptyBoard numMines generator 