-- part a N/A

{-
    (b) Consider the following function definitions:
            quadup n = n : quadup (n+4)
            take 0 xs = []
            take n (x:xs) = x : take (n-1) xs
        Show the evaluation of take 2 (quadup 10) using both Strict Evaluation and Lazy Evaluation.
        Show enough evaluation steps to either indicate the final result, or to illustrate why no such result will emerge.

        STRICT:
            take 2 (quadup 10)
            take 2 (10 : quadup (14))
            take 2 (10 : (14 : quadup (18)))
            take 2 (10 : (14 : (18 : (quadup (22)))))
            -- etc, quadup will keep going, for n += 4 as n approaches infinity

        LAZY:
            take 2 (quadup 10)
            take 2 (10 : quadup (14))
            10 : take 1 (quadup (14))
            10 : take 1 (14 : quadup (18))
            10 : (14 : take 0 (quadup (18))
            10 : (14 : [])
            10 : [14]
            [10, 14]
-}

{-
    (c) Write a function toDOS that takes a string representing a filename (root.ext) and converts it to one satisfying the DOS 8+3 format (All uppercase, root and extension with max length of 8 and 3 respectively).
        If the root or extension needs to be shortened, then they should be truncated. For example:
        toDOS "LongBigName.datafile" = "LONGBIGN.DAT"
-}

    toDOS :: String -> String

    toDOS (x:xs) = helperDOS (x:xs) 0 false 0 

    helperDOS :: String -> Int -> Bool -> Int -> String
    helperDOS [] _ _ _ = []                                                                 -- an empty string converts to an empty string
    helperDOS (x:xs) rootN pastDot extN =
        | pastDot = if (extN < 3)                                                           -- we have passed the dot
                        then ((toUpper x) : helperDOS xs rootN pastDot (extN+1))            -- the we are currently less than 3 characters into the extension, so continue appending
                    else []                                                                 -- we have appended three characters to the extension, nothing left to do
        | otherwise = if (x = '.')                                                          -- we have reached the dot
                        then x : (helperDOS xs rootN True extN)                             -- take note that we have passed the dot and continue appending
                    else                                                                    -- we haven't reached the dot, so are still in the root
                        if (rootN < 8)                                                      -- we haven't reached the max length of the root so keep appending
                            then (toUpper x) : helperDOS xs (rootN + 1) pastDot extN
                        else helperDOS xs rootN pastDot extN                                -- we have reached the max length of the root, don't append but call recursively as we still have to append the dot and the extension