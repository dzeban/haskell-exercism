-----------------------------------------------------------
---- |
---- Module: Strain
---- Description: Reimplementation of filter functions
---- Copyright: (c) 2015 Alex Dzyoba <alex.dzyoba@gmail.com>
---- License: MIT
-------------------------------------------------------------

module Strain where

keep :: (a -> Bool) -> [a] -> [a]
keep predicate [] = []
keep predicate (x:xs) 
    | predicate x = x : keep predicate xs
    | otherwise   = keep predicate xs

discard :: (a -> Bool) -> [a] -> [a]
discard f = keep (not . f)