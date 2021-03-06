module Board (
    Board (..), CellStatus (..),
    visible, flagged, hidden, questioned,
    isInBoard,
    initializeBoard,
    getCellValue, getCellStatus, updateCellStatus,
    expandCells,
    mine,
    incomplete, won, lost,
    updateGameState,
    uncoverClick, flagClick, unsureClick,
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
    numMines :: Int,
    state :: GameState
}

-- A mine cell and a blank cell represented by a function name, more intuitive than a magic number
mine, blank :: Proximity
mine = -1
blank = 0

-- Functions to access data constructors of CellStatus and GameState, in case their underlying representation is changed later on.
visible, hidden, flagged, questioned :: CellStatus
visible = Visible
hidden = Hidden 
flagged = Flagged
questioned = Questioned

incomplete, won, lost :: GameState
incomplete = Incomplete
won = Won 
lost = Lost 

isInBoard :: Board -> (Int, Int) -> Bool
isInBoard board (x, y) = 
    x >= 0 && x < width board && y >= 0 && y < height board

-- create a board with the given properties, where every cell is blank and hidden
createEmptyBoard :: Int -> Int -> Int -> Board
createEmptyBoard width height numMines = Board {
    cells = generate width (\_ -> Data.Vector.replicate height blank),
    statuses = generate width (\_ -> Data.Vector.replicate height Hidden),
    width = width,
    height = height,
    numMines = numMines,
    state = incomplete
}

getCellStatus :: Board -> (Int, Int) -> CellStatus
getCellStatus board (x, y) = statuses board ! x ! y

updateCellStatus :: (Int, Int) -> Board -> CellStatus -> Board
updateCellStatus (x, y) board status = 
    let 
        newRows = update (statuses board ! x) (singleton (y, status))   -- update row x with the value
        newStatuses = update (statuses board) (singleton (x, newRows))  -- update the cells with the new row x
    in board {statuses = newStatuses}                                   -- update the board with the new cells

getCellValue ::  Board -> (Int, Int) -> Proximity
getCellValue board (x,y) = 
    if isInBoard board (x,y) then
        cells board ! x ! y
    else
        -5

updateCellValue :: (Int, Int) -> Board -> Proximity -> Board
updateCellValue (x, y) board value = 
    let 
        newRows = update (cells board ! x) (singleton (y, value))   -- update row x with the value
        newCells = update (cells board) (singleton (x, newRows))    -- update the cells with the new row x
    in board {cells = newCells}                                     -- update the board with the new cells

incrementCellValue :: (Int, Int) -> Board -> Board
incrementCellValue coords board = 
    if isInBoard board coords
    then updateCellValue coords board $ getCellValue board coords + 1
    else board

-- increment the value of every cell that is adjacent to (x, y) and isn't a mine
updateAdjacentCells :: (Int, Int) -> Board -> Board
updateAdjacentCells (x,y) board = incrementCells [(x-1, y-1), (x,y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x,y+1), (x+1, y+1)] board
 where 
     incrementCells [] board = board
     incrementCells (head:tail) board = 
         if getCellValue board head == mine 
         then
            incrementCells tail board
         else
            incrementCellValue head $ incrementCells tail board


plantMine :: (Int, Int) -> Board -> Board
plantMine coords board = 
    let boardWithMinePlanted = updateCellValue coords board mine -- plant a mine at the coords
    in updateAdjacentCells coords boardWithMinePlanted           -- update adjecent cells

-- take a board and a number of mines
-- randomly generate an index and place a mine there, unless:
--  there is already a mine there
--  the index is the supplied index where mines are not allowed (the first click of the game)
populateBoard :: Board -> Int -> (Int, Int) -> StdGen -> Board
populateBoard board 0 _ _ = board
populateBoard board numMines coordsToSkip generator =
    let 
        (randomX, newGenerator) = randomR (0, width board-1) generator
        (randomY, newerGenerator) = randomR (0, height board-1) newGenerator
        in
            if getCellValue board (randomX, randomY) == mine || (randomX, randomY) == coordsToSkip
                then populateBoard board numMines coordsToSkip newerGenerator
                else 
                    let plantedBoard = plantMine (randomX, randomY) board  
                    in populateBoard plantedBoard (numMines-1) coordsToSkip newerGenerator

-- create an empty board with the given properties, then populate it
initializeBoard :: Int -> Int -> Int -> (Int, Int) -> StdGen -> Board
initializeBoard width height numMines coordsToSkip stdGen = 
    let emptyBoard = createEmptyBoard width height numMines
    in populateBoard emptyBoard numMines coordsToSkip stdGen 

-- uncover a cell that is hidden or questioned
-- if the cell value is 0, uncover all adjacent cells
expandCells :: (Int, Int) -> Board -> Board
expandCells (x, y) board =
    if isInBoard board (x, y) && (getCellStatus board (x, y) == hidden || getCellStatus board (x, y) == questioned)
    then 
        if getCellValue board (x, y) == blank then
            let updatedBoard = updateCellStatus (x, y) board visible
            in expandCells (x-1, y) $ expandCells (x, y-1) $ expandCells (x+1, y) $ expandCells (x, y+1) updatedBoard
        else updateCellStatus (x, y) board visible
    else board

-- take a board and check if the user has won or lost, updating the game state if so
updateGameState :: Board -> Board
updateGameState board = 
    let coordsList = [(columnIndices, rowIndices) | columnIndices <- [0 .. Board.width board - 1], rowIndices <- [0 .. Board.height board - 1]]
    in updateGameStateRecursive board won coordsList

-- iterate through the board to check the game state
updateGameStateRecursive :: Board -> GameState -> [(Int, Int)] -> Board
updateGameStateRecursive board state [] = board {state = state}

updateGameStateRecursive board Won (coords:coordsList)                              -- Assuming the game has been Won, until proven otherwise:
    | getCellStatus board coords == visible && getCellValue board coords == mine    --      If we reach an uncovered mine, game over. The game is lost
        = board {state = lost}
    | getCellStatus board coords /= visible && getCellValue board coords /= mine    --      If we reach an uncovered non-mine, we know the game is incomplete. We carry this knowledge until the end of the recursion, in case we reach the "Lost" condition.
        = updateGameStateRecursive board Incomplete coordsList
    | otherwise = 
        updateGameStateRecursive board Won coordsList                               --      We have not encountered a condition for "Lost" or "Incomplete", so assume we have Won and continue until proven otherwise.

updateGameStateRecursive board Incomplete (coords:coordsList) =                     --  Knowing that the game is incomplete:
   if getCellStatus board coords == visible && getCellValue board coords == mine    --      If we reach an uncovered mine, game over. The game is lost
    then board {state = lost}
    else updateGameStateRecursive board Incomplete coordsList                       --      We have not encountered a condition for "Lost". Continue the recursion in case we encounter it later.


-- Three high level functions to handle the click of a cell depending on the mouse mode:
uncoverClick :: Board -> (Int, Int) -> Board
uncoverClick board (x, y) = 
    let cellStatus = getCellStatus board (x, y) 
    in if cellStatus == visible 
        then board
        else  expandCells (x, y) $ updateCellStatus (x, y) board hidden

flagClick :: Board -> (Int, Int) -> Board
flagClick board (x, y) = 
    let cellStatus = getCellStatus board (x, y)
    in  if cellStatus == flagged
        then updateCellStatus (x, y) board hidden
        else if cellStatus == hidden || cellStatus == questioned
            then updateCellStatus (x, y) board flagged
            else board

unsureClick :: Board -> (Int, Int) -> Board
unsureClick board (x, y) = 
    let cellStatus = getCellStatus board (x, y) 
    in  if cellStatus == questioned
        then updateCellStatus (x, y) board hidden
        else if cellStatus == hidden || cellStatus == flagged
            then updateCellStatus (x, y) board questioned
            else board