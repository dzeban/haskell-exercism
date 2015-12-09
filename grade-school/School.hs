------------------------------------------------------------
---- |
---- Module: School
---- Description: School records accounting
---- Copyright: (c) 2015 Alex Dzyoba <alex.dzyoba@gmail.com>
---- License: MIT
------------------------------------------------------------

module School (School, empty, add, grade, sorted)
where

import Data.List (sort, nub)

type Grade = Int
type Name = String

data School = Empty | School [(Grade, Name)]
              deriving Show

-- | School type access.
--
-- Get the list of school records.  -- This is used instead of record syntax,
-- because we must return [] for Empty
--
list :: School -> [(Grade, Name)]
list Empty = []
list (School x) = x

-- | Return empty School. So sad...
empty :: School
empty = Empty

-- | Add a new student to a school grade
add :: Grade -> Name -> School -> School
add grade name Empty = School [(grade, name)]
add grade name school = School $ list school ++ [(grade, name)]

-- | List students in grade in alphabetical order
grade :: Grade -> School -> [Name]
grade inGrade school = sort [ n | (g, n) <- list school, g == inGrade ]

-- | List of school grades (sorted)
grades :: School -> [Grade]
grades s = nub . sort $ [ g | (g, n) <- list s ]

-- | Glue students list to the grade
studentsOfGrade :: School -> Grade -> (Grade, [Name])
studentsOfGrade school inGrade = (inGrade, grade inGrade school)

-- | Sort school records by grades AND names
sorted :: School -> [(Grade, [Name])]
sorted school = map (studentsOfGrade school) (grades school)

