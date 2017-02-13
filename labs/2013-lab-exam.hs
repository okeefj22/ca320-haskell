-- Q1
ans1 = [x | x <- [14,16..200], x `mod` 7 == 0]


-- Q2
inRange :: Int -> Int -> [Int]
inRange x y = [xy | xy <- [x..y]]


-- Q3
diff :: (Eq a) => [a] -> [a] -> [a]
diff [] ys = []
diff (x:xs) ys
    | x `elem` ys = diff xs ys
    | otherwise = x : (diff xs ys)


-- Q4
data BinTree t = Empty
               | Root t (BinTree t) (BinTree t)
                 deriving (Eq, Show)


-- Q5
postordTraversal :: BinTree a -> [a]
postordTraversal Empty = []
postordTraversal (Root n l r) = postordTraversal l ++ postordTraversal r ++ [n]
