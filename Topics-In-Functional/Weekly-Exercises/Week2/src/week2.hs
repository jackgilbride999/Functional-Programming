-- NB:
-- Uncomment the following line to test parts 1 and 2:
main = part2
-- Uncomment the following line to test part 3:
-- main = part3
-- Uncomment either of the following lines to test part 4 (These are simple test cases. The first will instantly terminate and the second will not terminate):
-- main = while (return False)
-- main = while (return True)
-- Uncomment the following lines to test part 5:
-- main = do
--    a <- read10
--    putStr a
{-
      PART 1: Write a function with this signature. The function should do _all_ 
      the IO operations in the provided list, in the order that they are provided.
      f1 :: [IO a] -> IO a
-}
-- Another way of writing this function signature is:
-- f1 :: [World -> (a, World)] -> (World -> (a, World))
-- The function is the following:
f1 :: [IO a] -> IO a
f1 (action:[]) = action
f1 (action:actions) = action >> f1 actions
-- This iterates through the list of IO actions, and evaluates them in order. 
-- The ">>" operation ensures that action is evaluated before f1 is called on actions.
-- The function must evaluate to IO a, so when we get to the last item in the list, 
-- we make the function evaluate to it. 


{-
      PART 2: What should this print?
-}
part2 = f1 actions where actions = [putChar 'h', putChar 'e', putChar 'l', putChar 'l', putChar 'o']
-- This prints "hello". It simply iterates through the list of IO items as described above, 
-- in order, which print the individual characters of the word "hello".


{-
      PART 3: How about this?
-}
part3 = do
            let   a = f1 actions
                  b = if True then putChar 'a' else putChar 'b'
            putStr "Hi there"
            where actions = [do putChar 'h', putChar 'e', putChar 'l', putChar 'l', putChar 'o']
-- This prints "Hi there". The function specifies what a, b and actions evaluate to, but never
-- actually uses them in its evaluation. The only functionality of the function is the line 
-- putStr "Hi there". This prints "Hi there" to the console. a, b or actions are never referenced,
-- so f1 and the putChar functions are never evaluated.

{-
      PART 4: Can you create a new Haskell Control structure, with this signature?
      It should perform the provided action repeatedly until the action returns false.
      while :: IO Bool -> IO ()
-}
-- Another way of writing this function signature is the following:
-- while :: (World -> (Bool, World)) -> (World -> (), World)
while :: IO Bool -> IO ()
while monad = do
      action <- monad
      if (action) 
            then (while monad) 
            else return ()
-- This evaluates the Bool in the IO Bool as "action". If action is true, the function
-- is called again with the same IO Bool. Through this recursion it will continually 
-- evaluate "action". If it ever evaluates as false, then the "else" is evaluated. 
-- "return ()" returns the unit type wrapped in the monad, i.e. returns "IO ()", which
-- is what is specified by the function header.

{-
      PART 5: Finally, how about writing
      f2 :: [IO a] -> IO [a]
      such that we can write read10 to read 10 characters
-}
read10 :: IO String
read10 = f2 $ take 10 actions
      where actions = getChar : actions

-- Another way of writing this function signature is the following:
-- f2 :: [World -> (a, World)] -> (World -> ([a], World))
f2 :: [IO a] -> IO [a]
f2 (monad:[]) = do
      action <- monad
      return (action:[])
f2 (monad:monads) = do
      action <- monad
      actions <- f2 monads
      return (action:actions)
-- This, like f1, iterates through a list of IO monads. Using the "do" notation, it
-- first evaluates the action in the monad at the head of the list. It then recursively
-- calls itself on the tail of the list, and extracts the list of actions from the monad
-- returned. It appends the head action to the list of actions. It uses "return" to wrap
-- this list of actions in an IO monad as required by the function header. 