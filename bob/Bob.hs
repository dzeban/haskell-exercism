-----------------------------------------------------------
-- |
-- Module: Bob
-- Description: A lackadaisical teenager implemntation
-- Copyright: (c) 2015 Alex Dzyoba <alex.dzyoba@gmail.com>
-- License: MIT
-----------------------------------------------------------

module Bob (responseFor) where

import Data.Char (toUpper, isSpace, isControl, isLetter)

data Tone = Question | Yell | None | Whatever
    deriving Show

-- | Check whether character is visible
isVisible :: Char -> Bool
isVisible c = not $ isSpace c || isControl c

-- | Check whether string is empty, i.e. doesn't contain visible chars
isEmpty :: String -> Bool
isEmpty s = not (any isVisible s)

-- | Check if string is a Yell: its letters is all capital
isYell :: String -> Bool
isYell s = map toUpper letters == letters
           && not (isEmpty letters) -- String may not contain letters
    where letters = filter isLetter s

-- | Determine Tone of the sentence
tone :: String -> Tone
tone s 
    | isEmpty s     = None
    | isYell s      = Yell
    | last s == '?' = Question
    | otherwise     = Whatever

-- | Generate Bob's response by determining Tone of the sentence
responseFor :: String -> String
responseFor req = case tone req of 
    None -> "Fine. Be that way!"
    Question -> "Sure."
    Yell -> "Whoa, chill out!"
    Whatever -> "Whatever."
