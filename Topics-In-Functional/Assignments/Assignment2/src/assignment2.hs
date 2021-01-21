import Graphics.UI.Threepenny
import Board
import Data.IORef
import System.Random

canvasSize = 400 :: Double

data Modes = Mine | Flag

main :: IO () 
main = do
    board <- initializeBoard 20 20 10
    startGUI defaultConfig setup

drawLine :: Element -> (Point, Point) -> UI ()
drawLine canvas (startPoint, endPoint) = do
    beginPath canvas
    moveTo startPoint canvas
    lineTo endPoint canvas
    closePath canvas
    stroke canvas

drawVerticalLines :: Element -> Point -> Double -> Double -> UI ()
drawVerticalLines canvas (x, y) cellWidth count = do
                                                    drawLine canvas ((x, 0), (x, canvasSize))
                                                    if count > 0 then
                                                        drawVerticalLines canvas (x+cellWidth, y) cellWidth (count - 1)
                                                    else
                                                        return ()

drawHorizontalLines :: Element -> Point -> Double -> Double -> UI ()
drawHorizontalLines canvas (x, y) cellHeight count = do
                                                    drawLine canvas ((0, y), (canvasSize, y))
                                                    if count > 0 then 
                                                        drawHorizontalLines canvas (x, y + cellHeight) cellHeight (count - 1)
                                                    else
                                                        return ()

detectClickedCell :: Point -> Int -> (Int, Int)
detectClickedCell (x,y) cellSize = (floor (x / fromIntegral cellSize),
                                        floor (y / fromIntegral cellSize))

setup window = do
    return window # set title "Minesweeper"

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
                getBody window #+ [string "pressed"]
            
            Flag -> do
                canvas # set' fillStyle (htmlColor "white")
                canvas # fillRect (x, y) 100 100
                getBody window #+ [string "flag"]