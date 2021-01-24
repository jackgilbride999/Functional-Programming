import Graphics.UI.Threepenny
import Board
import Data.IORef
import System.Random
import Control.Monad

canvasSize = 400 :: Double

data Modes = Mine | Flag

main :: IO () 
main = do
    startGUI defaultConfig setup

drawLine :: Element -> (Point, Point) -> UI ()
drawLine canvas (x1y1, x2y2) = do
    beginPath canvas
    moveTo x1y1 canvas
    lineTo x2y2 canvas
    closePath canvas
    stroke canvas

drawVerticalLines :: Element -> Point -> Double -> Double -> UI ()
drawVerticalLines canvas (x, y) cellWidth count = do
                                                    drawLine canvas ((x, 0), (x, canvasSize))
                                                    when (count > 0) $
                                                        drawVerticalLines canvas (x+cellWidth, y) cellWidth (count - 1)


drawHorizontalLines :: Element -> Point -> Double -> Double -> UI ()
drawHorizontalLines canvas (x, y) cellHeight count = do
                                                    drawLine canvas ((0, y), (canvasSize, y))
                                                    when (count > 0) $ 
                                                        drawHorizontalLines canvas (x, y + cellHeight) cellHeight (count - 1)

drawCells :: Element -> Board -> Double -> Double -> UI ()
drawCells canvas board cellWidth cellHeight = 
    drawCellsRecursive $ [drawCell canvas board cellWidth cellHeight (columnIndices, rowIndices) | columnIndices <- [0 .. Board.width board - 1], rowIndices <- [0 .. Board.height board - 1]]

drawCellsRecursive :: [UI ()] -> UI ()
drawCellsRecursive [] = return ()
drawCellsRecursive (x:xs) = do
    x
    drawCellsRecursive xs

drawCell :: Element -> Board -> Double -> Double -> (Int, Int) -> UI ()
drawCell canvas board cellWidth cellHeight (x, y) = do
    let
        canvasX = (fromIntegral x * cellWidth) + cellWidth / 2
        canvasY = fromIntegral y * cellHeight + cellHeight * 0.75
        cellValue = getCellValue (x, y) board
        cellStatus = getCellStatus (x, y) board
        in
            if cellStatus == visible then
            fillText (show cellValue) (canvasX, canvasY) canvas
            else if cellStatus == flagged 
                then fillText "F" (canvasX, canvasY) canvas
                else return ()


detectClickedCell :: Point -> Int -> IO (Int, Int)
detectClickedCell (x,y) cellSize = return (floor (x / fromIntegral cellSize),
                                        floor (y / fromIntegral cellSize))

setup window = do
    return window # set title "Minesweeper"

    canvas <- canvas
        # set Graphics.UI.Threepenny.height 400
        # set Graphics.UI.Threepenny.width 400
        # set style [("border", "solid black 1px"), ("background", "#eee")]
        # set textAlign Center 

    mode <- liftIO $ newIORef Mine
    pos <- liftIO $ newIORef (0, 0)
    board <- liftIO $ newIORef $ initializeBoard 20 20 10


    mineMode <- button #+ [string "Mine"]
    flagMode <- button #+ [string "Flag"]
    clearMode <- button #+ [string "Clear"]

    drawVerticalLines canvas (0, 0) (400 / 20) 20
    drawHorizontalLines canvas (0, 0) (400 / 20) 20

    getBody window #+
        [column [element canvas], element mineMode, element flagMode, element clearMode]

    on click clearMode $ const $ 
        canvas # clearCanvas

    on click mineMode $ \_ -> do
        liftIO $ writeIORef mode Mine

    on click flagMode $ \_ -> do
        liftIO $ writeIORef mode Flag

    on mousemove canvas $ \xy -> do
        liftIO $ writeIORef pos xy

    on click canvas $ \_ -> do
        (x, y) <- liftIO $ readIORef pos
        m <- liftIO $ readIORef mode
        boardValue <- liftIO $ readIORef board
        case m of
            Mine -> do
                coords <- liftIO $ detectClickedCell (x, y) (400 `Prelude.div` 20) 
                liftIO $ writeIORef board (updateCellStatus coords boardValue visible)
                return ()
            Flag -> do
                coords <- liftIO $ detectClickedCell (x, y) (400 `Prelude.div` 20) 
                liftIO $ writeIORef board (updateCellStatus coords boardValue flagged)
                return ()
        clearCanvas canvas
        drawVerticalLines canvas (0, 0) (400 / 20) 20
        drawHorizontalLines canvas (0, 0) (400 / 20) 20
        boardValue <- liftIO $ readIORef board
        drawCells canvas boardValue (400 / 20) (400 / 20)