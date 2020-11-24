# CourseWork for CSU34016 2019-20

Repository of college assignments created to introduce us to functional programming through Haskell.

## Exercise00

Simple introduction task to Haskell. Install stack, then compile and edit Haskell code to pass a simple test.

## Exercise01

Introduction to functions in Haskell. Implement the following functions:
- `Raise`, to convert a string to uppercase
- `nth`, to return the nth element of a list
- `commonLen`, to compare two sequences and report the length of the prefix they have in common

## Exercise02

Working with expressions. Given the folowing expression data type:
```haskell
data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)
}
```
along with a dictionary for values `type Dict k d  =  [(k,d)]`, a define function and a find function for the dictionary:
- Implement an eval function for a value, variable, addition, subtraction, multiplication, division and definition.
- Implement the following laws: 
    - `x + y = y + z`
    - `x + (y + z) = (x + y) + z`
    - `x - (y + z) = (x - y) - z`
    - `(x + y)\*(x - y) = x*x - y*y`

## Exercise03

Given a binary tree data type, association list binary type and lookup BST function:
- Implement insert BST function
- Implement function to convert association list to BST
- Implement function to convert BST to association list 

Use proof checking tool prfchk to prove the threorem `product ((1:2:[])++(3:4:[])) == 24`.