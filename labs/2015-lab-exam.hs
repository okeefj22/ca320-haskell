-- Q1
threes = [x | x <- [3,6..99], x `mod` 2 /= 0]


-- Q2
diff :: Int -> Int -> Int
diff x y
    | x < 0 || y < 0 = x + y
    | otherwise      = abs $ x - y


-- Q3
join :: Eq a => [a] -> [a] -> [a]
join [] ys        = ys
join xs []        = xs
join (x:xs) ys
    | x `elem` ys = join xs ys
    | otherwise   = x : (join xs ys)


-- Q4
data BinTree t = Empty | Root t (BinTree t) (BinTree t)
                 deriving (Eq, Ord, Show)


-- Q5
preorder :: (Ord a) => BinTree a -> [a]
preorder Empty               = []
preorder (Root t left right) = t : (preorder left ++ preorder right)
