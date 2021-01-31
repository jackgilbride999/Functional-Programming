import Graphics.UI.Threepenny
import Board
import AutoPlayer
import Data.IORef
import System.Random
import Control.Monad

data Modes = Mine | Flag | Unsure
data Difficulties = Beginner | Intermediate | Expert

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
                                                    drawLine canvas ((x, 0), (x, canvasWidth))
                                                    when (count > 0) $
                                                        drawVerticalLines canvas (x+cellWidth, y) cellWidth (count - 1)


drawHorizontalLines :: Element -> Point -> Double -> Double -> UI ()
drawHorizontalLines canvas (x, y) cellHeight count = do
                                                    drawLine canvas ((0, y), (canvasHeight, y))
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

detectClickedCell :: Point -> Double -> IO (Int, Int)
detectClickedCell (x,y) cellSize = return (floor (x / cellSize),
                                        floor (y / cellSize))

canvasWidth = 500 :: Double
canvasHeight = 500 :: Double

setup window = do
    return window # set title "Minesweeper"

    beginnerMode <- button #+ [string "Beginner"]
    intermediateMode <- button #+ [string "Intermediate"]
    expertMode <- button #+ [string "Expert"]

    getBody window #+
        [element beginnerMode, element intermediateMode, element expertMode]

    on click beginnerMode $ \_ -> do
        delete beginnerMode >> delete intermediateMode >> delete expertMode
        playGame window 8 8 10

    on click intermediateMode $ \_ -> do
        delete beginnerMode >> delete intermediateMode >> delete expertMode
        playGame window 16 16 40

    on click expertMode $ \_ -> do
        delete beginnerMode >> delete intermediateMode >> delete expertMode
        playGame window 24 24 99

playGame :: Window -> Double -> Double  -> Int -> UI ()
playGame window numRows numCols numMines = do

    mineButton <- button #+ [string "Mine"]
    flagButton <- button #+ [string "Flag"] 
    unsureButton <- button #+ [string "Unsure"] 
    autoPlayButton <- button #+ [string "AutoPlay"] # set style [("border", "solid black 1px")]

    updateButtonStyles mineButton flagButton unsureButton

    canvas <- canvas
        # set Graphics.UI.Threepenny.height (floor canvasHeight)
        # set Graphics.UI.Threepenny.width (floor canvasWidth)
        # set style [("border", "solid black 1px"), ("background", "#eee")]
        # set textAlign Center 

    stdGen <- liftIO newStdGen
    board <- liftIO $ newIORef $ initializeBoard (floor numRows) (floor numCols) numMines (0, 0) stdGen
    mode <- liftIO $ newIORef Mine
    pos <- liftIO $ newIORef (0, 0)
    gameStarted <- liftIO $ newIORef False

    drawVerticalLines canvas (0, 0) (canvasWidth / numCols) numCols
    drawHorizontalLines canvas (0, 0) (canvasHeight / numRows) numRows

    getBody window #+
        [element mineButton, element flagButton, element unsureButton, element autoPlayButton, column [element canvas]]

    on click mineButton $ \_ -> do
        liftIO $ writeIORef mode Mine
        updateButtonStyles mineButton flagButton unsureButton

    on click flagButton $ \_ -> do
        liftIO $ writeIORef mode Flag
        updateButtonStyles flagButton mineButton unsureButton

    on click unsureButton $ \_ -> do
        liftIO $ writeIORef mode Unsure
        updateButtonStyles unsureButton mineButton flagButton

    on click autoPlayButton $ \_ -> do
        boardValue <- liftIO $ readIORef board
        updatedBoard <- liftIO $ autoPlayerMove boardValue
        liftIO $ writeIORef board updatedBoard

    on mousemove canvas $ \xy -> do
        liftIO $ writeIORef pos xy

    on click canvas $ \_ -> do
        (x, y) <- liftIO $ readIORef pos
        coords <- liftIO $ detectClickedCell (x, y) (canvasHeight / numRows) 
        boardValue <- liftIO $ readIORef board
        started <- liftIO $ readIORef gameStarted
        
        when (not started && getCellValue coords boardValue == mine)
                (liftIO $ writeIORef board (initializeBoard (floor numRows) (floor numCols) numMines coords stdGen))

        liftIO $ writeIORef gameStarted True
        m <- liftIO $ readIORef mode
        boardValue <- liftIO $ readIORef board
        case m of
            Mine -> do
                liftIO $ writeIORef board (updateGameState (uncoverClick boardValue coords))
            Flag -> do
                liftIO $ writeIORef board (flagClick boardValue coords)
            Unsure -> do
                liftIO $ writeIORef board (unsureClick boardValue coords)
        clearCanvas canvas
        boardValue <- liftIO $ readIORef board
        if state boardValue == incomplete
            then do
                drawCells canvas boardValue (canvasHeight / numRows) (canvasWidth / numCols)
                drawVerticalLines canvas (0, 0) (canvasHeight / numCols) numCols
                drawHorizontalLines canvas (0, 0) (canvasWidth / numRows) numRows
            else if state boardValue == won 
                then do
                    set' fillStyle (htmlColor "green") canvas
                    fillRect (0,0) canvasHeight canvasWidth canvas
                    set' fillStyle (htmlColor "white") canvas
                    fillText "YOU WON" (0,0) canvas
                else do
                    set' fillStyle (htmlColor "red") canvas
                    fillRect (0,0) canvasHeight canvasWidth canvas
                    set' fillStyle (htmlColor "white") canvas
                    fillText "YOU LOST" (0,0) canvas

updateButtonStyles :: Element -> Element -> Element -> UI ()
updateButtonStyles button1 button2 button3 = do
    set' style [("border", "solid black 2px"), ("font-weight", "bold")] button1
    set' style [("border", "solid black 1px"), ("font-weight", "normal")] button2
    set' style [("border", "solid black 1px"), ("font-weight", "normal")] button3