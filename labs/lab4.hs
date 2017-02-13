-- Q1 implement your own version of built in functions
myAppend       :: [a] -> [a] -> [a]
myAppend [] [] = []
myAppend a []  = a
myAppend [] a  = a
myAppend a b   = myAppend (init a) ((last a) : b)

myHead        :: [a] -> a
myHead []     = error "Empty list has no head!"
myHead (x:xs) = x

myLast        :: [a] -> a
myLast []     = error "Empty list has no last!"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

myTail        :: [a] -> [a]
myTail []     = error "Empty list has no tail!"
myTail (_:xs) = xs

myInit        :: [a] -> [a]
myInit []     = error "Empty list has no init!"
myInit (x:[]) = []
myInit (x:xs) = x : (myInit xs)

myLength        :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

myReverse    :: [a] -> [a]
myReverse [] = []
myReverse xs = last xs : myReverse (init xs)

myConcat        :: [[a]] -> [a]
myConcat []     = []
myConcat (x:xs) = x ++ myConcat xs

mySum        :: Num a => [a] -> a
mySum []     = 0
mySum (x:xs) = x + mySum xs

myProduct        :: Num a => [a] -> a
myProduct []     = 1
myProduct (x:xs) = x * myProduct xs

myMaximum               :: Ord a => [a] -> a
myMaximum []            = error "No maximum of an empty list!"
myMaximum [x]           = x
myMaximum (x:xs@(y:ys))
    | x > y             = myMaximum $ x : ys
    | otherwise         = myMaximum xs

myMinimum               :: Ord a => [a] -> a
myMinimum []            = error "No maximum of an empty list!"
myMinimum [x]           = x
myMinimum (x:xs@(y:ys))
    | x < y             = myMinimum $ x : ys
    | otherwise         = myMinimum xs

myElem          :: Eq a => a -> [a] -> Bool
myElem x []     = False
myElem x (y:ys)
    | x == y    = True
    | otherwise = myElem x ys

myDelete          :: Eq a => a -> [a] -> [a]
myDelete x []     = []
myDelete x (y:ys)
    | x == y      = ys
    | otherwise   = y : myDelete x ys


-- Q2
myUnion           :: Eq a => [a] -> [a] -> [a]
myUnion x []      = x
myUnion [] x      = x
myUnion xs (y:ys)
    | y `elem` xs = myUnion xs ys
    | otherwise   = myUnion (xs ++ [y]) ys

myIntersect           :: Eq a => [a] -> [a] -> [a]
myIntersect x []      = []
myIntersect [] y      = []
myIntersect (x:xs) ys
    | x `elem` ys     = x : myIntersect xs ys
    | otherwise       = myIntersect xs ys
