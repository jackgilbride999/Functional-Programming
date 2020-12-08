{-
    A "Red-Black Tree" is a kind of binary search tree in which 
    node is labelled with a colour (either Red or Black), and 
    which is constructed such that the following invariants hold:
    - The root node, and all leaf nodes, are black
    - Each red node has black children
    - All paths from a node to any of it's descendant leaf nodes 
    will have an equal number of black nodes

    Write a declaration for a Haskell data type "RedBlack a" which 
    uses GADTs to ensure that the Red Black Tree invariants cannot 
    be violated.

    There is no need to include any operations on the tree.

    Hints:
    You will probably want to use the Peano arithmetic types (Zero
    and Succ) so that you have a type parameter that can be used to 
    mention the count of black nodes in a subtree.

    Your interal node type probably has three type parameters, one to 
    talk about the type actually stored in the tree, and two to do 
    with the invariants (colour and number of black nodes between this
    node between this node and the leaves under it).
-}
{-# LANGUAGE GADTs #-}
main = putStr "week10"

data Zero
data Succ n

data Red 
data Black 

data RedBlack a = forall n. Root (Node a Black n)

data Node a c n where 
    Leaf :: a -> Node a Black Zero
    RedNode :: a -> Node a Black n -> Node a Black n -> Node a Red n 
    BlackNode :: a -> Node a cl n -> Node a cr n -> Node a Black (Succ n)