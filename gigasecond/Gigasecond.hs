{-|
 - Module: Gigasecond
 - Description: Simple exercise for exercism.io, calculating gigasecond by date
 - Copyright: (c) 2015 Alex Dzyoba <alex.dzyoba@gmail.com>
 - License: MIT
-}
module Gigasecond where

import Data.Time.Clock (UTCTime, addUTCTime)

fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime (10^9)
