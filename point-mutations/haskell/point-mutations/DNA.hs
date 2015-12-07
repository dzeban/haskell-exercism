-----------------------------------------------------------
---- |
---- Module: DNA
---- Description: Calculate hamming distance between DNA strands
---- Copyright: (c) 2015 Alex Dzyoba <alex.dzyoba@gmail.com>
---- License: MIT
-------------------------------------------------------------

module DNA where

dnaEquals :: String -> String -> [Bool]
dnaEquals = zipWith (==)

hammingDistance :: String -> String -> Int
hammingDistance dna1 dna2 = length mutatedAcids
    where mutatedAcids = filter (==False) (dnaEquals dna1 dna2)