-- Q1
data BinTree t = Empty
               | Node t (BinTree t) (BinTree t)
                 deriving (Eq, Show)

leaf x = Node x Empty Empty

-- (a)
addnode :: Ord a => a -> BinTree a -> BinTree a
addnode x Empty          = leaf x
addnode x t@(Node n l r)
    | x < n              = Node n (addnode x l) r
    | x > n              = Node n l (addnode x r)
    | otherwise          = t

-- (b)
maketree :: Ord a => [a] -> BinTree a
maketree []     = Empty
maketree [x]    = leaf x
maketree (x:xs) = addnode x $ maketree xs

-- (c)
inorder :: Ord a => BinTree a -> [a]
inorder Empty                = []
inorder (Node n Empty Empty) = [n]
inorder (Node n l r)         = inorder l ++ (n : inorder r)

-- (d)
mpsort :: Ord a => [a] -> [a]
mpsort a = inorder $ maketree a
