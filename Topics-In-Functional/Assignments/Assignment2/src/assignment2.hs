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

detectClickedCell :: Point -> Int -> IO (Int, Int)
detectClickedCell (x,y) cellSize = return (floor (x / fromIntegral cellSize),
                                        floor (y / fromIntegral cellSize))

setup window = do
    return window # set title "Minesweeper"

    board <- liftIO $ initializeBoard 20 20 10

    canvas <- canvas
        # set height 400
        # set width 400
        # set style [("border", "solid black 1px"), ("background", "#eee")]

    mode <- liftIO $ newIORef Mine
    pos <- liftIO $ newIORef (0, 0)

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
        case m of
            Mine -> do
                canvas # set' fillStyle (htmlColor "black")
                canvas # fillRect (x, y) 100 100
                coords <- liftIO $ detectClickedCell (x, y) (400 `Prelude.div` 20) 
                board <- liftIO $ updateCellStatus coords board visible
                getBody window #+ [string $ show coords]
            Flag -> do
                canvas # set' fillStyle (htmlColor "white")
                canvas # fillRect (x, y) 100 100
                coords <- liftIO $ detectClickedCell (x, y) (400 `Prelude.div` 20) 
                board <- liftIO $ updateCellStatus coords board flagged 
                getBody window #+ [string $ show coords]