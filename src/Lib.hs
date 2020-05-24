module Lib
    ( someFunc
    ) where

import Data.Time          -- For the UTCTime type
import Data.Time.ISO8601  -- Parse the datetime format in gpx files
import Data.Time.Clock
import System.Time.Utils (renderSecs)

import Gpx

renderS = renderSecs . round :: NominalDiffTime -> String

showResult :: Result -> String
showResult (d,t) = "(" ++ (show d) ++ "Km, " ++(renderS t) ++ ")"

someFunc = do
            route <- getRoute "activity.gpx"
            putStrLn $ show $ totalDistance route
            putStrLn $ renderS $ totalTime route
            putStrLn $ showResult $ fastestNk 5 (trackPoints route)
