-- Name: Jack Joseph Gilbride,  Username: gilbridj
module Ex01 where
import Data.Char (toUpper) -- needed for Part 1

{- Part 1

Write a function 'raise' that converts a string to uppercase

Function 'toUpper :: Char -> Char' converts a character to uppercase
if it is lowercase. All other characters are unchanged

-}
raise [] = []
raise (x:xs) = (toUpper x):(raise xs) 

{- Part 2

Write a function 'nth' that returns the nth element of a list

-}
nth :: Int -> [a] -> a
nth 1 (x:xs) = x
nth y (x:xs) = nth (y-1) xs

{- Part 3

write a function commonLen that compares two sequences
and reports the length of the prefix they have in common.

-}
commonLen :: Eq a => [a] -> [a] -> Int
--commonLen _ _ = error "commonLen not yet implemented"
commonLen (x:xs) (y:ys) = if x == y then )+1 else 0
commonLen _ _ = 0(commonLen xs ys