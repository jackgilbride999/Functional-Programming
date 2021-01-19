module Board(
    Board,
    createBoard
) where

import Data.Vector

data Board = Board {
    cells :: Vector (Vector Int),
    width :: Int,
    height :: Int
}

createBoard :: Int -> Int -> Board
createBoard width height = Board (generate width (\_ -> Data.Vector.replicate height 0)) width height

getCellValue :: (Int, Int) -> Board -> Int
getCellValue (x,y) board = (cells board) ! x ! y

updateCellValue :: (Int, Int) -> Board -> Int -> Board
updateCellValue (x, y) board value = 
    let 
        newRows = update ((cells board) ! x) (singleton (y, value)) -- update the row vector with the value
        newCells = update (cells board) (singleton (x, newRows))    -- update the cells with the new row vector
    in Board newCells (width board) (height board)                  -- update the board with the new cells
