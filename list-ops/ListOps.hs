------------------------------------------------------------
---- | 
---- Module: ListOps
---- Description: Basic list operations (almost) without prelude
---- Copyright: (c) 2015 Alex Dzyoba <alex.dzyoba@gmail.com>
---- License: MIT
------------------------------------------------------------

module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' = foldl

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f i [] = i
foldr f i (x:xs) = x `f` foldr f i xs

length :: [a] -> Int
length xs = sum (map (const 1) xs)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter f (x:xs) = [x | f x] ++ filter f xs

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

concat :: [[a]] -> [a]
concat = foldr (++) []
