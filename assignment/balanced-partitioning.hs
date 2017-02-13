-- Jacob O'Keeffe
-- 13356691

import Data.List (delete, sort)

-- takes a list xs and returns a tuple containing balanced partitions of xs 
balancedPartition :: [Int] -> ([Int],[Int])
balancedPartition [] = ([],[])
balancedPartition xs = (xxs, (otherPartition xs xxs))
  where
    xxs = bPartition xs

-- takes a list xs and a list ys
-- returns xs less the elements from ys
otherPartition :: [Int] -> [Int] -> [Int]
otherPartition xs []     = xs
otherPartition xs (y:ys) = otherPartition (delete y xs) ys 

-- sorts the elements in reverse order for use with bPartition'
-- this new list is passed, along with a target value, to bPartition' 
bPartition :: [Int] -> [Int]
bPartition [] = []
bPartition xs = bPartition' (reverse $ sort xs) target
  where
    target = (sum xs) `quot` 2

-- returns a partition with a sum as close to target as possible
bPartition' :: [Int] -> Int -> [Int]
bPartition' [] _           = []
bPartition' _ 0            = []
bPartition' (x:xs) target
  | x == target            = [x]
  | x < target             = x : bPartition' xs (target - x)
  | otherwise              = bPartition' xs target