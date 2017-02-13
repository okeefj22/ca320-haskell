data AVLTree t = Empty
               | Root t (AVLTree t) (AVLTree t)
                 deriving (Eq, Show)

leaf x = Root x Empty Empty

balanceLL :: (Ord a) => AVLTree a -> AVLTree a
balanceLL (Root n (Root n1 l1 r1) r) = Root n1 l1 (Root n r1 r)

balanceRR :: (Ord a) => AVLTree a -> AVLTree a
balanceRR (Root n l (Root n1 l1 r1)) = Root n1 (Root n l l1) r1

--balanceRl Root n l (Root n1 l1 r1) = Root l1
--showTree :: (Ord a) => AVLTree a -> String
showTree Empty = ""
showTree (Root x l r) = ("[ " ++ x ++ "] " ++ (showTree l) ++ (showTree r))