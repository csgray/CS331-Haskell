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
-- Takes two lists of the same type and returns the index of the first element in the second list
-- which starts a contiguous sublist of the first list.
-- If the first list is not found as a contiguous sublist of the second, the return value is Nothing.
findList :: Eq a => [a] -> [a] -> Maybe Int
findList [] _  = Just 0
findList xs ys = let n = 0 in listMatch xs ys n where
  listMatch xs ys n
    | length xs > length ys     = Nothing                        -- if xs is larger than ys then it can't be a sublist
    | take (length xs) ys == xs = Just n                         -- if the first X elements of the second list are the same as the search list return the index
    | otherwise                 = listMatch xs (tail ys) (n + 1) -- otherwise run again but lop off the first character of the search list and increment the index

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
-- Takes a list of numbers and returns a tuple of two numbers consisting of
-- the sum of the even-index items and the sum of the odd-index items
sumEvenOdd :: Num a => [a] -> (a, a)
sumEvenOdd _ = (0, 0)  -- DUMMY; REWRITE THIS!!!
