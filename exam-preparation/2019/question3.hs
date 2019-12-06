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
}