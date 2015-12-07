-----------------------------------------------------------
---- |
---- Module: Anagram
---- Description: Find anagrams
---- Copyright: (c) 2015 Alex Dzyoba <alex.dzyoba@gmail.com>
---- License: MIT
-------------------------------------------------------------

module Anagram where
import Data.List (sort)
import qualified Data.Text as T

toLower :: String -> String
toLower s = T.unpack $ T.toLower $ T.pack s

anagram :: String -> String -> Bool
anagram s1 s2 = toLower s1 /= toLower s2 && -- avoid anagram of itself case
                (sort (toLower s1) == sort (toLower s2))

anagramsFor :: String -> [String] -> [String]
anagramsFor string = filter (anagram string)