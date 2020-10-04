{-
    Consider the following function definitions:
        f1 [] = 1
        f1 (x:xs) = x * f1 xs
        f2 [] = 0
        f2 (x:xs) = 1 + f2 xs
        f3 [] = 0
        f3 (x:xs) = x + f3 xs
        f4 [] = []
        f4 (x:xs) = x ++ f4 xs
        f5 [] = 0
        f5 (x:xs) = (x*x) + f5 xs
    They all have a common pattern of behaviour
-}

{-
    (a) Write a higher-order function hof that captures this common behaviour.
-}

    hof emptyResponse firstElem op [] = emptyResponse
    hof emptyResponse firstElem op (x:xs) = firstElem 'op' (hof emptyResponse firstElem op xs)

{-
    (b) Rewrite each of f1, f2, ... above to be a call to hof with appropriate arguments
-}
    f1 = hof 1 x (*)
    f2 = hof 0 1 (+)
    f3 = hof 0 x (+)
    f4 = hof [] x (++)
    f5 = hof 0 (x*x) (+)

{-
    (c) We have a binary tree built from number-string pairs, ordered by the number (acting as a key):

    data Tree = Empty
            | Single Int String
            | Many Tree Int String Tree
    
    and one function search defined over it:

    search :: Tree -> Int -> String

    search x (Many left i s right)
        | x == i = s
        | x > i = search x right
    search x (Single i s)
        | x == i = s

    Explain the ways in which function search can fail with Haskell runtime errors

    -- It will fail if we call Search x Empty, as there is no case to handle this
    -- It will fail if we call Search x (Many left i s right) and x < i as there is no guard to handle this case
    -- It will fail if we call Search x (Single i s) and x > i or x < i as there are no guards to handle either of these cases

    -}
