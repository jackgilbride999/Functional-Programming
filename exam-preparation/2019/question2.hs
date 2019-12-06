{-
    We have a binary search tree built from number-string pairs, ordered by the number (acting as key),

    data BinTree = BNil
                    | BOne Int String
                    | BTwo BinTree Int String BinTree

    and one lookup partially defined over it:

    lookup :: BinTree -> Int -> String

    lookup (BOne i s) x
        | x == i = s

    lookup (BTwo left i s right) x
        | x < i = lookup left x
        | x > i = lookup right x
-}

{-
    (a) Describe the three ways in which the function lookup can fail, with Haskell runtime pattern-matching errors
    
    - lookup BNil will fail because there is no lookup function which takes lookup BNil
    - lookup (BOne i s) x will fail when x is not equal to i because there is no case to handle this
    - lookup (BTwo left i s right) x will fail when x is equal to i because there is no case to handle this
-}

{-
    (b) Add in error handling for function lookup above, using the Maybe type, to ensure this function is now total.
        Note that this will require changing the type of the lookup function. Your answer should give the new type.
-}

    data BinTree = BNil
                    | BOne Int String
                    | BTwo BinTree Int String BinTree

    lookup :: BinTree -> Int -> Maybe String

    lookup BNil x = Nothing

    lookup (BOne i s) x
        | x == i = Just s
        | otherwise = Nothing

    lookup (BTwo left i s right) x
        | x < i = lookup left x
        | x > i = lookup right x
        | otherwise = Just s        -- x == i so return x

{-
    (c) Add in generic error handling for the lookup function above, using monads, ensuring it is now total, and giving back a useful error message.
        Note that this will also require changing the type (again) of the lookup function, and your answer should give this revised type.
-}
    lookup :: Monad m => BinTree -> Int -> m String
    
    lookup BNil x = fail "Lookup reached an empty binary tree node. No value was found for the key"

    lookup (BOne i s) x 
        | x == i = return s
        | otherwise = fail "Lookup reached a leaf which does not contain the specified key. No value was found for the key"

    lookup (BTwo left i s right) x
        | x < i = lookup left x
        | x > i = lookup right x
        | otherwise = return s