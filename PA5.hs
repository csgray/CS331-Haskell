-- PA5.hs
-- Corey S. Gray
-- 21 Mar 2018
--
-- For CS F331 / CSCE A331 Spring 2018
-- Solutions to Assignment 5 Exercise B

module PA5 where

collatz :: Integer -> Integer
collatz n
  | odd n     = 3 * n + 1 
  | otherwise = n `div` 2

collatzSequence :: Integer -> [Integer]
collatzSequence n
  | n == 1    = 1 : []
  | otherwise = n : collatzSequence (collatz n)

-- collatzCounts
collatzCounts :: [Integer]
collatzCounts = map collatzLength [1..] where
  collatzLength n = toInteger (length (collatzSequence n) - 1)





-- findList
findList :: Eq a => [a] -> [a] -> Maybe Int
findList _ _ = Just 42  -- DUMMY; REWRITE THIS!!!


-- operator ##
(##) :: Eq a => [a] -> [a] -> Int
_ ## _ = 42  -- DUMMY; REWRITE THIS!!!


-- filterAB
filterAB :: (a -> Bool) -> [a] -> [b] -> [b]
filterAB _ _ bs = bs  -- DUMMY; REWRITE THIS!!!


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

