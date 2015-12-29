----------------------------------------------------------------------------
---- | 
---- Module: ETL
---- Description: Transform scrabble scores from [(score: [letter])] map to 
----              [(letter:score)] map
---- Copyright: (c) 2015 Alex Dzyoba <alex@dzyoba.com>
---- License: MIT
----------------------------------------------------------------------------

module ETL (transform) 
where

import qualified Data.Map as M
import qualified Data.Text as T

type OldScores = M.Map Int [String]
type NewScores = M.Map String Int

-- | Transform old scores to the new one
---- We iterate over key-value pairs and fold them into empty map of new scores
transform :: OldScores -> NewScores
transform old = M.foldWithKey transformEntry M.empty old

-- | Transform single entry of old scores
---- This iterates over letters and fold them into newScores
transformEntry :: Int -> [String] -> NewScores -> NewScores
transformEntry score letters newScores = foldr (updateWithScore score) newScores letters

-- | Insert single letter with score into map of new scores. 
---- Letter is converted to lowercase
updateWithScore :: Int -> String -> NewScores -> NewScores
updateWithScore score letter newScores = M.insert (toLower letter) score newScores

-- | Little helper to convert string to lowercase with help of Data.Text
toLower :: String -> String
toLower = T.unpack . T.toLower . T.pack

