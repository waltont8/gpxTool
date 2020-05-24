module Lib
    ( someFunc
    ) where

import Data.Time          -- For the UTCTime type
import Data.Time.ISO8601  -- Parse the datetime format in gpx files
import Data.Time.Clock
import System.Time.Utils (renderSecs)
import System.Environment

import Gpx

renderS = renderSecs . round :: NominalDiffTime -> String

showResult :: Result -> String
showResult (d,t) = "(" ++ (show d) ++ "Km, " ++(renderS t) ++ ")"

someFunc = do
            args <- getArgs
            route <- getRoute $ if length args > 0 then head args else "activity.gpx"
            putStrLn $ "Total distance " ++ (show $ totalDistance route)
            putStrLn $ "Total time " ++ (renderS $ totalTime route)
            putStrLn $ "Best 1k " ++ (showResult $ fastestNk 1 (trackPoints route))
            putStrLn $ "Best 5k " ++ (showResult $ fastestNk 5 (trackPoints route))
            putStrLn $ "Best 10k " ++ (showResult $ fastestNk 10 (trackPoints route))
            putStrLn $ "Best mile " ++ (showResult $ fastestNk 1.609344 (trackPoints route))
