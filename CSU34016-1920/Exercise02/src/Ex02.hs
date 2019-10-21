{- gilbridj Jack Joseph Gilbride -}
module Ex02 where
import Data.List ((\\))
import Data.Maybe

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !

-- Any time we say Id, we can effectively substitute it with String
type Id = String

-- Expr data type
data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)

-- a dictionary maps keys to datum values
-- an obvious approach is to use a list of key/datum pairs
type Dict k d  =  [(k,d)]

-- defining a link between a key and datum is simply cons-ing such a pair onto the start of the list
define :: Dict k d -> k -> d -> Dict k d
define d s v = (s,v):d

-- find simply searches along the list
find :: Eq k => Dict k d -> k -> Maybe d
find []             _                 =  Nothing
find ( (s,v) : ds ) name | name == s  =  Just v
                         | otherwise  =  find ds name

-- a dictionary where the key is a string and the datum is a double
type EDict = Dict String Double

-- sample data
v42 = Val 42 ; j42 = Just v42

-- Part 1 : Evaluating Expressions -- (60 test marks, worth 15 Exercise Marks) -

-- Implement the following function so all 'eval' tests pass.

-- eval should return Nothing if:
  -- (1) a divide by zero operation was going to be performed;
  -- (2) the expression contains a variable not in the dictionary.

eval :: EDict -> Expr -> Maybe Double
eval d (Val x) = Just x
eval d (Var x) = if(isJust(find d x)) then (find d x) else Nothing
eval d (Add x y) = if (isJust(eval d x)) && (isJust(eval d y)) then Just (fromJust(eval d x)+fromJust(eval d y)) else Nothing
eval d (Sub x y) = if (isJust(eval d x)) && (isJust(eval d y)) then Just (fromJust(eval d x)-fromJust(eval d y)) else Nothing
eval d (Mul x y) = if (isJust(eval d x)) && (isJust(eval d y)) then Just (fromJust(eval d x)*fromJust(eval d y)) else Nothing
eval d (Dvd x (Val 0)) = Nothing
eval d (Dvd x y) = if (isJust(eval d x)) && (isJust(eval d y)) then Just (fromJust(eval d x)/fromJust(eval d y)) else Nothing
-- The intended meaning of Def x e1 e2 is; x is in scope in e2, but not in e1
-- computer value if e1, and assign value to x, then evaluate e2 as overall result

eval d (Def x e1 e2) = if isJust(eval d e1) then eval (define d x (fromJust(eval d e1)) ) e2 else Nothing
eval d e = Nothing

-- Part 2 : Expression Laws -- (15 test marks, worth 15 Exercise Marks) --------

{-

There are many, many laws of algebra that apply to our expressions, e.g.,

  x + y            =  y + z         Law 1
  x + (y + z)      =  (x + y) + z   Law 2
  x - (y + z)      =  (x - y) - z   Law 3
  (x + y)*(x - y)  =  x*x - y*y     Law 4
  ...

  We can implement these directly in Haskell using Expr

  Function LawN takes an expression:
    If it matches the "shape" of the law lefthand-side,
    it replaces it with the corresponding righthand "shape".
    If it does not match, it returns Nothing

    Implement Laws 1 through 4 above
-}


law1 :: Expr -> Maybe Expr
law1 (Add x y) = Just (Add y x)
law1 e = Nothing

law2 :: Expr -> Maybe Expr
law2 (Add x (Add y z)) = Just (Add (Add x y) z)
law2 e = Nothing

law3 :: Expr -> Maybe Expr
law3 (Sub x (Add y z)) = Just (Sub (Sub x y) z)
law3 e = Nothing

law4 :: Expr -> Maybe Expr
law4 (Mul (Add x y) (Sub x' y'))  = if x == x' && y == y' then Just (Sub (Mul x x) (Mul y y)) else Nothing
law4 e = Nothing