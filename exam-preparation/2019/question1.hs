{-
    Give a complete implementation of the Prelude functions described below.
    By "complete" is meant that any other functions used to help implement those below must also have their implementations given.
-}

{-
    (a) Returns the first element of a list, if it is non empty, with a runtime error otherwise.
-}
        head :: [a] -> a
        head [] = error "Illegal argument: cannot return the head of an empty list"
        head (x:xs) = x

{-
    (b) Returns the last element of a list, if it is non-empty, with a runtime error otherwise
-}
        last :: [a] -> a
        last [] = error "Illegal argument: cannot return the tail of an empty list"
        last [x]    = x
        last (x:xs) = last xs

{-
    (c) Concatenate two lists together
-}
        (++) :: [a] -> [a] -> [a]
        [] ++ ys = ys
        (x:xs) ++ (ys) = x:(xs ++ ys)

{-
    (d) Reverse its list argument
-}
        reverse :: [a] -> [a]
        reverse [] = []
        reverse (x:xs) = (reverse xs) ++ x

{-
    (e) Uses a predicate to split a list in two,
            the first list being the longest prefix that satisfies the predicate,
            while the second list is what remains
-}
        span :: (a -> Bool) -> [a] -> ([a], [a])
        span f [] = ([], [])
        span f (x:xs) = if f x then (x:ys, zs) else ([], zs)
            where (ys, zs) = span f xs

{-
    (f) Compute the minimum of a non-empty list
-}
        minimum :: Ord a => [a] -> a
        minimum [] = error "Illegal argument: cannot return the minimum of an empty list"
        minimum [x] = x
        minimum (x:xs) = if x < y then x else y
            where y = minimum xs