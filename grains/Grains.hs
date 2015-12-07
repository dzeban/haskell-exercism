{-|
 - Module: Grains
 - Description: Simple exercise for exercism.io, calculating grains on chess board
 - Copyright: (c) 2015 Alex Dzyoba <alex.dzyoba@gmail.com>
 - License: MIT
-}
module Grains where

square :: (Integral a) => a -> a
square n = 2^(n - 1)

total :: (Integral a) => a
total = sum [ square x | x <- [1..64] ]
