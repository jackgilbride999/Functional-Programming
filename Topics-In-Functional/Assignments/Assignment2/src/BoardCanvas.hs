module BoardCanvas(
    BoardCanvas,
    newBoardCanvas
)
where

import Graphics.UI.Threepenny
import Board

data BoardCanvas = BoardCanvas {
    canvasElement :: Element,
    canvasWidth :: Double,
    canvasHeight :: Double,
    cellWidth :: Double,
    cellHeight :: Double
}

newBoardCanvas :: Element -> Double -> Double -> Board -> BoardCanvas
newBoardCanvas canvas cellWidth cellHeight board = BoardCanvas {
    canvasElement = canvas,
    canvasWidth = cellWidth * fromIntegral (Board.width board),
    canvasHeight = cellWidth * fromIntegral (Board.height board),
    cellWidth = cellWidth,
    cellHeight = cellHeight
}