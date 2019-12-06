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
    helperDOS [] _ _ _ = [] -- an empty string converts to an empty string
    helperDOS (x:xs) rootN pastDot extN =
        | pastDot = if (extN < 3) then ((toUpper x) : helperDOS xs rootN pastDot (extN+1)) else []    -- we have passed the dot, if the extension length is <3, continue appending, otherwise, ignore the rest of inpput
        | otherwise = if (x = '.') then x : (helperDOS xs rootN True extN)  -- if we reach the dot, set dotPassed to true and continue appenfine
                        else if (rootN < 8) then (toUpper x) : helperDOS xs (rootN + 1) pastDot extN -- if we haven't passed the dot and the root lenght is <8, continue appending
                            else helperDOS xs rootN pastDot extN                                     -- otherwise do not append but keep going as we must eventually append the dot and extension   
