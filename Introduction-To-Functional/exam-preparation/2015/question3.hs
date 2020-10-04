{-
    (a) We have an expression datatype as follows:
    
    data Expr = K Int
                | V String
                | Add Expr Expr
                | Dvd Expr Expr
                | Let String Expr Expr

    and a dictionary type with insert (ins) and lookup (lkp functions) (full code not given):
    
    type Dict = [(String, Int)]
    ins :: String -> Int -> Dict -> Dict
    lkp :: String -> Dict -> Maybe Int
    
    and one function eval defined over expressions:

    eval :: Dict -> Expr -> Int
    eval _ (K i) = i
    eval d (V s) = fromJust $ lkp s d
    eval d (Add e1 e2) = eval d e1 + eval d e2
    eval d (Dvd e1 e2) = eval d e1 'div' eval d e2
    eval d (Let v e1 e2) = eval (ins v i d) e2
                            where i = eval d e1
    fromJust (Just x) = x

    Add in error handling for function eval above, using the Maybe type, to ensure this function is now total.
    Note that this will require changing the type of this function.
-}

    eval :: Dict -> Expr -> Maybe Int
    eval _ (K i) = Just i
    eval d (V s) = lkp s d
    eval d (Add e1 e2) 
        = case (eval d e1, eval d e2) of 
            (Just m, Just n) -> Just (m*n)
            _       -> Nothing
    eval d (Dvd e1 e2)
        = case (eval d e1, eval d e2) of 
            (Just m, Just n) -> if n == 0 then Nothing else Just (m/n)
            _   -> Nothing

{-
    (b) Not applicable
-}