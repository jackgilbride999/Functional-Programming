{- 
generate a list of size height, 
where each element is a list of size width
-}
minesweeper height width numBombs = putStr "hi"

-- function to create a h x w list where each element is -1
generateBoard :: Int -> Int -> [[Int]]
generateBoard h w = replicate h $ replicate w -1

-- take a board and a number of bombs and fill the board with that many bombs
populateBoard board h w numBombs = "todo"

-- take a board already populated with bombs and populate each square with the number of adjacent bombs
generateDistances board h w = "todo"