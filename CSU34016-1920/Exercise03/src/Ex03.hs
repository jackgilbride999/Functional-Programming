{- gilbridj Jack Joseph Gilbride -}
module Ex03 where
import Data.List ((\\))

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !

-- Binary Tree
{-  
    Binary tree data type. Takes two arguments of type a and b. Is either a leaf, 
    or a branch. A branch holds a left subtree, a key of type a, a value of type b 
    and a right subtree.
-} 
data BT a b
  = Leaf
  | Branch (BT a b) a b (BT a b)
  deriving (Eq, Show)

-- association list
{-
  List of key value pairs, where keys are of type a and values are of type b.
-}
type Assoc a b = [(a,b)]

-- lookup binary (search) tree
{-
  Function to search binary tree. Takes a BST (where keys are of type a1 and 
  values are of type a) along with a value of type a1. Returns a Maybe a, which
  is Just a if the value is found, and Nothing otherwise. a1 is restricted to
  be an ordered type so we can compare keys.
-}
lkpBST :: Ord a1 => BT a1 a -> a1 -> Maybe a
lkpBST Leaf _  =  Nothing
lkpBST (Branch left k d right) k'
 | k < k'     =  lkpBST left k'
 | k > k'     =  lkpBST right k'
 | otherwise  =  Just d

-- Coding Part 1 (13 Marks)

-- insert into binary (search) tree
{-
    Takes a key of type a, value of type b, and a BST which holds keys of type
    a and values of type b. Returns the BST with the key value pair inserted.
    a is retricted to an orderable type so that it can be compared.
-}
insBST :: Ord a => a -> b -> BT a b -> BT a b
insBST _ _ _  =  error "insBST not yet implmented"

-- Coding Part 2 (6 Marks)

-- convert an association list to a binary search tree
{-
    Takes an association list and returns an equivalent BST, where both hold keys 
    of type a and values of type b. a is retricted to an orderable type so that it 
    can be compared.
-}
assoc2bst :: Ord a => Assoc a b -> BT a b
assoc2bst _ = error "assoc2bst not yet implemented"

-- Coding Part 3 (6 Marks)

-- convert a binary search tree into an (ordered) association list
{-
  Takes a BST and returns an equivalent association list, where both hold keys of type
  c and values of type e. c is retricted to an orderable type so that it can be compared.
-}
bst2assoc :: Ord c =>  BT c e -> Assoc c e
bst2assoc _ = error "bst2assoc not yet implemented"
