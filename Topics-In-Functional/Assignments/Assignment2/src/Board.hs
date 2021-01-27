module Board (
    Board (..),
    CellStatus (..),
    visible,
    flagged,
    getCellValue, 
    initializeBoard,
    updateCellStatus,
    getCellStatus,
    expandCells,
    mine,
    questioned,
    incomplete,
    won,
    lost,
    updateGameState
) where
import Data.Vector
import System.Random

{-
    The underlying data types for the board.
        Proximity represents the number of bombs adjacent to a cell, and is equivalent to an Int.
        CellStatus represents how the cell will be presented to the viewer. A cell may be Visible to or Hidden from the user. 
            Additionally the user may Flag the cell, or place an "Unsure" symbol on it. These statuses are also represented in the data type.
        GameState represents the overall state of the game. A game is complete when the user has Won or Lost. If they have not reached either
            of these states, the GameState is Incomplete.
        The Board structure, represented by a record type, containing all of the information for the board. Including:
            cells - a 2D structure to store if each cell in the board is a bomb, and otherwise the number of adjacent cells that are bombs.
                Vectors were chosen instead of Lists as they are easier and clearer to index into, and a 2D structure requires a lot of indexing.
            statuses - a 2D structure to store the CellStatus of each game in the board. Each element of statuses corresponds to the equivalent 
                element in cells.
            width - the number of columns in the board (the size of the outer vector in cells and statuses).
            height - the number of rows in the board (the size of the inner vectors in cells and statuses).
            state - the value of the GameState.
-}
type Proximity = Int
data CellStatus = Visible | Hidden | Flagged | Questioned deriving (Eq, Show)
data GameState = Incomplete | Won | Lost deriving (Eq, Show)
data Board = Board {
    cells :: Vector (Vector Proximity),
    statuses :: Vector (Vector CellStatus),
    width :: Int,
    height :: Int,
    state :: GameState
}

-- A mine cell and a blank cell represented by a function name, more intuitive than a magic number
mine = -1           :: Proximity
blank = 0           :: Proximity

-- Functions to access data constructors of CellStatus and GameState, in case their underlying representation is changed later on.
visible = Visible   :: CellStatus
hidden = Hidden     :: CellStatus
flagged = Flagged   :: CellStatus
questioned = Questioned  :: CellStatus

incomplete = Incomplete :: GameState
won = Won :: GameState
lost = Lost :: GameState

createEmptyBoard :: Int -> Int -> Board
createEmptyBoard width height = Board {
    cells = generate width (\_ -> Data.Vector.replicate height blank),
    statuses = generate width (\_ -> Data.Vector.replicate height Hidden),
    width = width,
    height = height,
    state = incomplete
}

-- todo add checks to see if outside of board
getCellValue :: (Int, Int) -> Board -> Proximity
getCellValue (x,y) board = 
    if isInBoard (x,y) board then
        cells board ! x ! y
    else
        -5

updateCellValue :: (Int, Int) -> Board -> Proximity -> Board
updateCellValue (x, y) board value = 
    let 
        newRows = update (cells board ! x) (singleton (y, value)) -- update row x with the value
        newCells = update (cells board) (singleton (x, newRows))    -- update the cells with the new row x
    in board {cells = newCells} -- update the board with the new cells

isInBoard :: (Int, Int) -> Board -> Bool
isInBoard (x, y) board = 
    x >= 0 && x < width board && y >= 0 && y < height board

incrementCellValue :: (Int, Int) -> Board -> Board
incrementCellValue coords board = 
    if isInBoard coords board
    then updateCellValue coords board $ getCellValue coords board + 1
    else board

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
        (randomX, newGenerator) = randomR (0, width board-1) generator
        (randomY, newerGenerator) = randomR (0, height board-1) newGenerator
        in
            if getCellValue (randomX, randomY) board == mine
                then populateBoard board numMines newerGenerator
                else 
                    let plantedBoard = plantMine (randomX, randomY) board  
                    in populateBoard plantedBoard (numMines-1) newerGenerator

initializeBoard :: Int -> Int -> Int -> StdGen -> Board
initializeBoard width height numMines stdGen = 
    let emptyBoard = createEmptyBoard width height
    in populateBoard emptyBoard numMines stdGen 

updateCellStatus :: (Int, Int) -> Board -> CellStatus -> Board
updateCellStatus (x, y) board status = 
    let 
        newRows = update (statuses board ! x) (singleton (y, status)) -- update row x with the value
        newStatuses = update (statuses board) (singleton (x, newRows))    -- update the cells with the new row x
    in board {statuses = newStatuses} -- update the board with the new cells

getCellStatus :: (Int, Int) -> Board -> CellStatus
getCellStatus (x, y) board = statuses board ! x ! y

expandCells :: (Int, Int) -> Board -> Board
expandCells (x, y) board =
    if isInBoard (x, y) board && getCellStatus (x, y) board /= visible
    then 
        if getCellValue (x, y) board == blank then
            let updatedBoard = updateCellStatus (x, y) board visible
            in expandCells (x-1, y) $ expandCells (x, y-1) $ expandCells (x+1, y) $ expandCells (x, y+1) updatedBoard
        else updateCellStatus (x, y) board visible
    else board

updateGameState :: Board -> Board
updateGameState board = 
    let coordsList = [(columnIndices, rowIndices) | columnIndices <- [0 .. Board.width board - 1], rowIndices <- [0 .. Board.height board - 1]]
    in updateGameStateRecursive board won coordsList

updateGameStateRecursive :: Board -> GameState -> [(Int, Int)] -> Board
updateGameStateRecursive board state [] = board {state = state}
updateGameStateRecursive board Incomplete (coords:coordsList) = 
   if getCellStatus coords board == visible && getCellValue coords board == mine
    then board {state = lost}
    else updateGameStateRecursive board Incomplete coordsList
updateGameStateRecursive board Won (coords:coordsList) 
    | getCellStatus coords board == visible && getCellValue coords board == mine
        = board {state = lost}
    | getCellStatus coords board == hidden && getCellValue coords board /= mine
        = updateGameStateRecursive board Incomplete coordsList
    | otherwise = 
        updateGameStateRecursive board Won coordsList
   