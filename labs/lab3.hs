-- Q1
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []     = True
isPalindrome [x]    = True
isPalindrome (x:xs)
    | x == last xs  = isPalindrome (init xs)
    | otherwise     = False


-- Q2
shortest :: [[a]] -> [a]
shortest []               = []
shortest [x]              = x
shortest (x:ys@(y:xs))
    | length x < length y = shortest (x : xs)
    | otherwise           = shortest ys


-- Q3
type Poly = [Int]

sumPoly :: Poly -> Poly -> Poly
sumPoly [] []         = []
sumPoly a []          = a
sumPoly [] a          = a
sumPoly (x:xs) (y:ys) = (x + y) : sumPoly xs ys


-- Q4
evalPoly :: Int -> [Int] -> Int
evalPoly _ [] = 0
evalPoly x xs = ((last xs) * (x ^ ((length xs) - 1))) + evalPoly x (init xs)

evalPoly' :: Int -> [Int] -> Int
evalPoly' _ []     = 0
evalPoly' x (y:ys) = y + (x * (evalPoly' x ys))