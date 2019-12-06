{-
    Give a complete implementation of the Prelude functions described below.
    By "complete" is meant that any other functions used to help implement those below must also have their implementations given.
-}

{-
    (a) Returns the firs element of a list, if it is non-empty, with a runtime error otherwise.
-}
        head :: [a] -> a
        head [] = error "Invalid argument. Attempted to call head on an empty list"
        head (x:xs) = x

{-
    (b) Returns everything but the last element of a list, if it is non-empty, with a runtime error otherwise
-}
        init :: [a] -> [a]
        init [] = error "Invalid argument. Attempted to call init on an empty list"
        init [x] = []
        init (x:xs) = x : init xs

{-
    (c) Returns the last element of a list, if it is non-empty, with a runtime error otherwise
-}
        last :: [a] -> a
        last [] = error "Invalid argument. Attempted to call last on an empty list"
        last [x] = x
        last (x:xs) = last xs

{-
    (d) Uses a predicate to split a list into two, 
            the first list being the longest prefix that satisfies the predicate,
            while the second list is what remains
-}
        span :: (a -> Bool) -> [a] -> ([a],[a])
        span f [] = ([], [])
        span f (x:xs)  
            | f x = (x:ys, zs)
            | otherwise = (ys, zs)
            where (ys, zs) = span f xs

{-
    (e) We can index into a list, starting from zero.
        So xs!!n returns the (n+1)th element of xs, provided n is non negative and the length of the list is long enough.
        Otherwise, we get a runtime error.
-}
        (!!) :: [a] -> Int -> a
        xs !! n
            | n < 0 = error "Invalid argument. Specified index was less than zero"
            | otherwise = nth n 0 xs

        nth n count [] = error "Invalid argument. Specified index was too big for the list"    
        nth n count (x:xs)
            | n == count = x 
            | otherwise = nth n (count+1) xs

{-
    (f) Take a binary function and a non-empty list of elements and use the function to reduce the list down to one value with nesting to the left,
            as illustrated immediately below
            foldl1 op [x1,x2,...,xn-1,xn]
                = ((...((x1 'op' x2) 'op' x3)...) 'op' xn-1) 'op' xn
-}
        foldl1 :: (a -> a -> a) -> [a] -> a

        foldl1 [] = "Invalid argument. foldl1 must take a non empty list of elements."
        foldl1 (x:xs) f = foldhelper x xs f
            
        foldhelper a [x] f = f a x
        foldhelper a (x:xs) f = foldhelper (f a x) xs f
