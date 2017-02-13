-- Q1: list of odd numbers from 1 to 99
ls = [1,3..99]


-- Q2: list of even numbers not evenly divisible by 3 and whose sum is less than 1000
whileSumLessThan :: [Int] -> Int -> Int -> [Int]
whileSumLessThan (x:xs) i n
    | (x + i) < n = x : whileSumLessThan xs (i + x) n
    | otherwise   = []

ls' = whileSumLessThan [x | x <- [2,4..], x `mod` 3 /= 0] 0 1000


-- Q3: function to return a list of elements in the first list but not in the second list
diff :: (Eq a) => [a] -> [a] -> [a]
diff [] ys = []
diff (x:xs) ys
    | x `elem` ys = diff xs ys
    | otherwise   = x : diff xs ys


-- Q4: data type for binary trees
data BinTree t = Empty
               | Root t (BinTree t) (BinTree t)
                 deriving (Eq, Show)


-- Q5: function to add an element to a binary tree
addElem :: (Ord a) => BinTree a -> a -> BinTree a
addElem Empty x          = (Root x Empty Empty)
addElem t@(Root n l r) x
    | x < n              = Root n (addElem l x) r
    | x > n              = Root n l (addElem r x)
    | otherwise          = t
