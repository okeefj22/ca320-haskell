## Balanced Partition in Haskell

Write a Haskell program that calculates a _balanced partition_ of _N_ items, stored in an array, where each item has a value between 0 and _K_ such that the difference between the sum of the values of first partition, _S1_, and the sum of the values of the second partiton, _S2_, is minimised. Each partition does not have to have the same number of elements.  

For example, given the array [1,2,3,4,5], the balanced partitions could be [1,3,4] and [2,5], or [1,2,4] and [3,5], or [1,2,5] and [3,4].  

One classical way to solve this is to use _dynamic programming_. For _dynamic programming_ to be efficient you should avoid recalculating intermediate results. This can be tricky in a functional language as it does not store state information. The solution is _data memoisation_. Intermediate results are stored is a data structure when they are initially calculated and then simply retrived when needed.  

Here is an example of _data memoisation_ in calculating a Fibonacci number.

```Haskell
import Data.Array  

fibonacci :: Integer -> Integer  
fibonacci n = memo!n  
where  
  memo = array (0, n) [ (i, fib i) | i <- [0..n] ]  
  fib 0 = 0  
  fib 1 = 1  
  fib i = memo!(i-1) + memo!(i-2)
```

This example uses the _Array_ module. Since Haskell is a _lazy_ programming language it only calculates a function when it is needed.

Implement a solution to the _balanced partition_ in Haskell.
