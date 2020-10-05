main = print $ flatten (Node (Node (Node (Leaf 1) (Leaf 2)) (Node (Node (Leaf 3) (Leaf 4))(Node (Leaf 5) (Leaf 6)))) (Node (Leaf 7) (Leaf 8)))

data Tree a =   Leaf a
                | Node (Tree a) (Tree a)
                deriving Show

count:: Tree a -> Integer
count (Leaf _) = 1
count (Node left right) = (count left) + (count right)

depth:: Tree a -> Integer
depth (Leaf _) = 1
depth (Node left right) = max (depth left) (depth right)

flatten:: Tree a -> [a]
flatten (Leaf x) = x:[]
flatten (Node left right) = (flatten left) ++ (flatten right)