module WordCount
where

import qualified Data.Map as Map
import Data.Char
import Data.Maybe

wordCount :: String -> Map.Map String Int
wordCount = count . words . map removeSpecialChars

removeSpecialChars :: Char -> Char
removeSpecialChars c = if isAlphaNum c then toLower c
                       else ' '

isWord :: Char -> Bool
isWord c = isAlphaNum c || isSpace c

count :: [String] -> Map.Map String Int
count = foldr tableUpdate Map.empty

tableUpdate :: String -> Map.Map String Int -> Map.Map String Int
tableUpdate s table = if isNothing oldVal then
                          Map.insert s 1 table
                      else
                          Map.insert s (fromJust oldVal + 1) table
                      where
                          oldVal = Map.lookup s table

