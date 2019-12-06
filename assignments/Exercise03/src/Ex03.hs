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
insBST k' d' Leaf = Branch Leaf k' d' Leaf        -- if we hit a lead (empty) node, make it a branch with key/value pair
insBST k' d' (Branch left k d right)              -- if we hit a branch:
 | k' < k = Branch (insBST k' d' left) k d right  -- if key is less than tree's key, recurse on left subtree and return
 | k' > k = Branch left k d (insBST k' d' right)  -- if key is greater than tree's key, recurse on right subtree and return
 | otherwise = Branch left k' d' right            -- otherwise, put new key value pair in current branch and return

-- Coding Part 2 (6 Marks)

-- convert an association list to a binary search tree
{-
    Takes an association list and returns an equivalent BST, where both hold keys 
    of type a and values of type b. a is retricted to an orderable type so that it 
    can be compared.
-}
assoc2bst :: Ord a => Assoc a b -> BT a b
assoc2bst [] = Leaf                                 -- if passed an empty list, return the equivalent for a BST which is a leaf
assoc2bst ((a, b):xs) = insBST a b (assoc2bst xs)   -- otherwise, run recursively on the list and insert the key and value

-- Coding Part 3 (6 Marks)

-- convert a binary search tree into an (ordered) association list
{-
  Takes a BST and returns an equivalent association list, where both hold keys of type
  c and values of type e. c is retricted to an orderable type so that it can be compared.
-}
bst2assoc :: Ord c =>  BT c e -> Assoc c e

bst2assoc Leaf =  []                                                                  -- if passed a leaf, return the equivalent list which is empty
bst2assoc (Branch left k d right) = ((bst2assoc left) ++ (k,d):(bst2assoc right))     -- return ordered list from left ++ current pair ++ ordered list from right
