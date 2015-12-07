{-|
Module : Sublist
Description : Determine sublists and superlists
Copyright : (c) Alex Dzyoba <alex.dzyoba@gmail.com>, 2015
License : MIT
-}
module Sublist where

data Sublist = Equal | Sublist | Superlist | Unequal 
     deriving (Eq, Show)

-- | Check whether two lists are equal
isEqual :: (Eq a) => [a] -> [a] -> Bool
isEqual xs ys = xs == ys

-- | Check whether the first list is a sublist of second list 
isSublist :: (Eq a) => [a] -> [a] -> Bool
isSublist [] _  = True
isSublist _ []  = False
isSublist xs ys 
    | length xs > length ys            = False 
    | isEqual xs (take (length xs) ys) = True
    | otherwise = isSublist xs (drop 1 ys)

-- | Check whether the first list is a superlist of second list
isSuperlist :: (Eq a) => [a] -> [a] -> Bool
isSuperlist xs ys = isSublist ys xs

-- | Compare two lists to determine sublist type
sublist :: (Eq a) => [a] -> [a] -> Sublist
sublist xs ys 
    | isEqual xs ys     = Equal
    | isSublist xs ys   = Sublist
    | isSuperlist xs ys = Superlist
    | otherwise         = Unequal
