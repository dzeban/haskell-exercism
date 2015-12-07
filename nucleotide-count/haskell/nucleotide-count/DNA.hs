-----------------------------------------------------------
---- |
---- Module: DNA
---- Description: Count nucleotids in DNA
---- Copyright: (c) 2015 Alex Dzyoba <alex.dzyoba@gmail.com>
---- License: MIT
-------------------------------------------------------------

module DNA where

data Nucleotide = Adenine | Cytosine | Guanine | Thymine 
    deriving (Eq, Show)

type DNA = [Nucleotide]

count :: Char -> String -> Int
count c s = countNucleotide (toNucleotide c) (toDna s)

countNucleotide :: Nucleotide -> DNA -> Int
countNucleotide n dna = length $ filter (==n) dna

toNucleotide :: Char -> Nucleotide
toNucleotide 'A' = Adenine
toNucleotide 'C' = Cytosine
toNucleotide 'G' = Guanine
toNucleotide 'T' = Thymine
toNucleotide  x  = error $ "invalid nucleotide " ++ show x 

toDna :: String -> [Nucleotide]
toDna = map toNucleotide

nucleotideCounts :: String -> [(Char, Int)]
nucleotideCounts s = [
    ('A', count 'A' s),
    ('T', count 'T' s),
    ('C', count 'C' s),
    ('G', count 'G' s)
    ]