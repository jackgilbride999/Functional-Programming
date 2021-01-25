module BoardCanvas(
    BoardCanvas,
    newBoardCanvas
)
where

import Graphics.UI.Threepenny
import Board

data BoardCanvas {
    canvas :: Element,
    canvasWidth :: Double,
    canvasHeight :: Double,
    cellWidth :: Double,
    cellHeight :: Double
}

newBoardCanvas :: Element -> Double -> Double -> Board -> BoardCanvas
newBoardCanvas canvas cellWidth cellHeight board = BoardCanvas {
    canvas = canvas,
    canvasWidth = cellWidth * Board.width board,
    canvasHeight = cellWidth * Board.height board,
    cellWidth = cellWidth,
    cellHeight = cellHeight
}