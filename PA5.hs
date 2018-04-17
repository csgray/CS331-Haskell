-- PA5.hs
-- Corey S. Gray
-- 16 April 2018
--
-- For CS F331 / CSCE A331 Spring 2018
-- Solutions to Assignment 5 Exercise B

module PA5 where

-- collatzCounts
-- Takes an integer n and returns a list of length n with each element in the list 
-- being how many iterations of the Collatz function are required to take index + 1 to 1
collatzCounts :: [Integer]
collatzCounts = map collatzLength [1..] where
  collatzLength n = toInteger (length (collatzSequence n) - 1)
  collatzSequence n
    | n == 1    = 1 : []
    | otherwise = n : collatzSequence (collatz n)
  collatz n
    | odd n     = 3 * n + 1 
    | otherwise = n `div` 2

-- findList
findList :: Eq a => [a] -> [a] -> Maybe Int
findList list1 list2 = Just 42  -- DUMMY; REWRITE THIS!!!

-- operator ##
-- Takes two lists of the same type and
-- returns the number of indices at which the two lists contain equal values
(##) :: Eq a => [a] -> [a] -> Int
[] ## _ = 0
_ ## [] = 0
(x:xs) ## (y:ys)
  | x == y    = xs ## ys + 1
  | otherwise = xs ## ys 

-- filterAB
-- Takes a Boolean function and two lists then returns a list of all items
-- for which the corresponding item in the first list makes the Boolean function true.
filterAB :: (a -> Bool) -> [a] -> [b] -> [b]
filterAB condition [] _ = []
filterAB condition _ [] = []
filterAB condition (x:xs) (y:ys)
  | condition x = y : filterAB condition xs ys
  | otherwise   = filterAB condition xs ys

-- sumEvenOdd
sumEvenOdd :: Num a => [a] -> (a, a)
{-
  The assignment requires sumEvenOdd to be written using a fold.
  Something like this:

    sumEvenOdd xs = fold* ... xs where
        ...

  Above, "..." should be replaced by other code. The "fold*" must be
  one of the following: foldl, foldr, foldl1, foldr1.
-}
sumEvenOdd _ = (0, 0)  -- DUMMY; REWRITE THIS!!!

