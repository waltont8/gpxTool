module Lib
    ( someFunc
    ) where

import Data.Time          -- For the UTCTime type
import Data.Time.ISO8601  -- Parse the datetime format in gpx files
import Gpx

someFunc = do
            pts <- getTrackPoints "activity.gpx"
            putStrLn $ show $ time $ head pts
            putStrLn $ show $ secondsSince ( time $ head pts) (time $ head $ reverse $ pts)
