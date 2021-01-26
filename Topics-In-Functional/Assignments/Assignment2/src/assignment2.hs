import Graphics.UI.Threepenny
import Board
import Data.IORef
import System.Random
import Control.Monad

canvasSize = 400 :: Double

data Modes = Mine | Flag | Unsure

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
    sequence_  $ [drawCell canvas board cellWidth cellHeight (columnIndices, rowIndices) | columnIndices <- [0 .. Board.width board - 1], rowIndices <- [0 .. Board.height board - 1]]

drawCell :: Element -> Board -> Double -> Double -> (Int, Int) -> UI ()
drawCell canvas board cellWidth cellHeight (x, y) = do
    let
        canvasX = (fromIntegral x * cellWidth)
        digitX = canvasX + cellWidth / 2
        canvasY = fromIntegral y * cellHeight
        digitY = canvasY + cellHeight * 0.75
        cellValue = getCellValue (x, y) board
        cellStatus = getCellStatus (x, y) board
        in
            if cellStatus == visible then
                if cellValue == mine then
                    drawMine canvas (canvasX, canvasY) (digitX, digitY) cellWidth cellHeight
                else drawDigit canvas (digitX, digitY) cellValue
            else if cellStatus == flagged then 
                drawFlag canvas (canvasX, canvasY) (digitX, digitY) cellWidth cellHeight
            else if cellStatus == questioned  then
                drawUnsure canvas (canvasX, canvasY) (digitX, digitY) cellWidth cellHeight
            else
                return ()

drawDigit :: Element -> (Double , Double) -> Int -> UI()
drawDigit canvas canvasCoords digit = do
    case digit of
        1 -> set' fillStyle (htmlColor "blue") canvas
        2 -> set' fillStyle (htmlColor "green") canvas
        3 -> set' fillStyle (htmlColor "red") canvas
        4 -> set' fillStyle (htmlColor "purple") canvas
        5 -> set' fillStyle (htmlColor "black") canvas
        6 -> set' fillStyle (htmlColor "gray") canvas
        7 -> set' fillStyle (htmlColor "maroon") canvas
        8 -> set' fillStyle (htmlColor "turquoise") canvas
        _ -> set' fillStyle (htmlColor "black") canvas
    fillText (show digit) canvasCoords canvas

drawMine :: Element -> (Double, Double) -> (Double, Double) -> Double -> Double -> UI()
drawMine canvas topLeftCoords centerCoords cellWidth cellHeight = do
    set' fillStyle (htmlColor "red") canvas
    fillRect topLeftCoords cellWidth cellHeight canvas
    set' fillStyle (htmlColor "white") canvas
    fillText "M" centerCoords canvas

drawFlag :: Element -> (Double, Double) -> (Double, Double) -> Double -> Double -> UI()
drawFlag canvas topLeftCoords centerCoords cellWidth cellHeight = do
    set' fillStyle (htmlColor "purple") canvas
    fillRect topLeftCoords cellWidth cellHeight canvas
    set' fillStyle (htmlColor "white") canvas
    fillText "F" centerCoords canvas

drawUnsure :: Element -> (Double, Double) -> (Double, Double) -> Double -> Double -> UI()
drawUnsure canvas topLeftCoords centerCoords cellWidth cellHeight = do
    set' fillStyle (htmlColor "green") canvas
    fillRect topLeftCoords cellWidth cellHeight canvas
    set' fillStyle (htmlColor "white") canvas
    fillText "?" centerCoords canvas

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
    stdGen <- liftIO newStdGen
    board <- liftIO $ newIORef $ initializeBoard 20 20 50 stdGen

    mineMode <- button #+ [string "Mine"]
    flagMode <- button #+ [string "Flag"]
    unsureMode <- button #+ [string "Unsure"]
    clearMode <- button #+ [string "Clear"]

    drawVerticalLines canvas (0, 0) (400 / 20) 20
    drawHorizontalLines canvas (0, 0) (400 / 20) 20

    getBody window #+
        [column [element canvas], element mineMode, element flagMode, element unsureMode, element clearMode]

    on click clearMode $ const $ 
        canvas # clearCanvas

    on click mineMode $ \_ -> do
        liftIO $ writeIORef mode Mine

    on click flagMode $ \_ -> do
        liftIO $ writeIORef mode Flag

    on click unsureMode $ \_ -> do
        liftIO $ writeIORef mode Unsure

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
                liftIO $ writeIORef board (expandCells coords boardValue)
                return ()
            Flag -> do
                coords <- liftIO $ detectClickedCell (x, y) (400 `Prelude.div` 20) 
                liftIO $ writeIORef board (updateCellStatus coords boardValue flagged)
                return ()
            Unsure -> do
                coords <- liftIO $ detectClickedCell (x, y) (400 `Prelude.div` 20) 
                liftIO $ writeIORef board (updateCellStatus coords boardValue questioned)
                return ()                
        clearCanvas canvas
        boardValue <- liftIO $ readIORef board
        drawCells canvas boardValue (400 / 20) (400 / 20)
        drawVerticalLines canvas (0, 0) (400 / 20) 20
        drawHorizontalLines canvas (0, 0) (400 / 20) 20