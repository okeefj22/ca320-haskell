-- Q1
data BinTree t = Empty | Root t (BinTree t) (BinTree t)
                 deriving (Eq, Ord, Show)

leaf x = Root x Empty Empty

-- (a)
addnode :: Ord a => a -> BinTree a -> BinTree a
addnode x Empty        = leaf x
addnode x (Root n l r)
    | x < n            = (Root n (addnode x l) r)
    | otherwise        = (Root n l (addnode x r))

-- (b)
maketree :: Ord a => [a] -> BinTree a
maketree (x:[]) = leaf x
maketree (x:xs) = addnode x $ maketree xs

-- (c)
inorder :: BinTree a -> [a]
inorder Empty        = []
inorder (Root n l r) = inorder l ++ (n : inorder r)

-- (d)
mpsort :: Ord a => [a] -> [a]
mpsort xs = inorder $ maketree xs


-- Q7
hoaddnode :: Ord a => (a -> a -> Bool) -> a -> BinTree a -> BinTree a
hoaddnode _ x Empty         = leaf x
hoaddnode fn x (Root n l r)
    | fn x n                = (Root n (hoaddnode fn x l) r)
    | otherwise             = (Root n l (hoaddnode fn x r))

homaketree :: Ord a => (a -> a -> Bool) -> [a] -> BinTree a
homaketree _ (x:[])  = leaf x
homaketree fn (x:xs) = hoaddnode fn x $ homaketree fn xs

hosort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
hosort fn xs = inorder $ homaketree fn xs
