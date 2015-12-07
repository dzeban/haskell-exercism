-- |
-- Module: SumOfMultiples
-- Description: Find sum for list of multiples in a list of numbers
-- Copyright: (c) 2015 Alex Dzyoba <alex.dzyoba@gmail.com>
-- License: MIT
--

module SumOfMultiples where

-- | Check whether a value is divisible by any number in a list
isDiv :: (Integral a) => [a] -> a -> Bool
isDiv ms x
    | 0 `elem` remainders = True
    | otherwise = False
    where remainders = map (rem x) ms

-- | Compute the sum of multiples in a list 
sumOfMultiples :: (Integral a) => [a] -> a -> a
sumOfMultiples ms n = sum (filter (isDiv ms) [1..(n-1)])

-- | Deduced "default"
sumOfMultiplesDefault = sumOfMultiples [3,5,6]
