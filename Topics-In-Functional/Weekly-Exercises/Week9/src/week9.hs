import Text.Read
import System.IO
import Control.Concurrent
import Control.Monad
{-
    In this exercise you will write a very simple server which will asynchronously compute and 
    deliver values for the user.

    The goal is to write a small interactive program which will use forkIO and Channels to coordinate 
    worker threads that will compute values in the background.

    The user will provide input data, or request a result. Each time they supply data a worker thread 
    is spawned (use forkIO) which will write a result back to a channel (keep a channel that’s shared 
    between all worker threads and the main thread).

    Each time the user request a result the first item in the channel is printed back (the program 
    can block if there’s no result in the channel yet).

    To keep it simple we will have only three things the user can provide as input:

    A number – this will trigger a new thread to calculate a value (use some long-ish running 
    computation – maybe computing the factorial of the square of the number entered, or finding the 
    prime factors of the number or something like that. Use some expensive to calculate function that 
    you already have, or find in the the textbooks. It’s not important what it does so long as it 
    takes a while.
    
    A string saying “result”. This will print the result of one of the completed threads from the 
    channel (blocking if necessary)

    A string saying “quit”, which will end the program. It would be nice if any results currently in 
    the channel were printed, but that’s a bonus. You can use “interact” from the Prelude to construct 
    the main loop, or you can just write a recursive function that reads and acts on an input string. 
    Feel free to make simplifying assumptions in this exercise, the main thing is to get some 
    experience using forkIO and channels.
-}
main = do
    hSetBuffering stdout NoBuffering    -- turn off buffering so each thread prints immediately
    channel <- newChan                  -- create the single channel that will be used by all threads
    handleInput channel                 -- call the recursive function which handles user input

handleInput :: Chan Double -> IO()
handleInput channel = do
    userInput <- getLine
    case userInput of
        "quit" -> do
            --chanContents <- getChanContents channel               -- ### Uncomment the line to the left to print the contents of the channel before quitting. This means that the program will wait for the threads to terminate, so that it can print the results, before it terminates.
            print chanContents
        _ -> do
            case userInput of
                "result" -> do
                    result <- readChan channel
                    print (result)
                _ -> case readMaybe userInput :: Maybe Double of    -- the input is not "quit" or "result", so is it a number?
                    Just a -> do
                        forkIO (worker channel a)                   -- the input is a number, so spawn a worker thread for it
                        return ()
                    Nothing -> putStrLn "Invalid input!"
            handleInput channel -- recursive call as long as the user hasn't entered "quit"

worker :: Chan Double -> Double -> IO()
worker channel input = let
    rounded =  fromIntegral $ round (input)     -- round the number so it makes sense to get its fib
    in writeChan channel (fib rounded)

fib :: Double -> Double
fib n | n < 2 = 1
fib n = fib (n-1) + fib (n-2)