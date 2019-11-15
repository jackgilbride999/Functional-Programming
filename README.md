# CourseWork for CSU34016 2019-20

Repository of college assignments created to introduce us to functional programming through Haskell.

## Exercise00

Exercise to introduce us to Haskell. Involved the installing stack onto our machines before compilng and editing Haskell code to pass a simple test.

## Exercise01

Our introduction to functions in Haskell. Our tasks were to implement the functions:
- "Raise", which converts a string to uppercase
- "nth", which returns the nth element of a list
- "commonLen", which compares two sequences and reports the length of the prefix they have in common

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
along with a dictionary for values 'type Dict k d  =  [(k,d)]', a define function and a find function for the dictionary:
- Implement an eval function for a value, variable, addition, subtraction, multiplication, division and definition.
- Implement the following laws: 
    - x + y = y + z
    - x + (y + z) = (x + y) + z
    - x - (y + z) = (x - y) - z
    - (x + y)\*(x - y) = x*x - y*y

## Exercise03

Given a binary tree data type, association list binary type and lookup BST function:
- implement insert BST function
- implement function to convert association list to BST
- implement function to convert BST to association list 

Use proof checking tool prfchk to prove the threorem 'product ((1:2:[])++(3:4:[])) == 24'.