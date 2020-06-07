module Lib
    ( someFunc
    ) where

import System.Environment

import Gpx


someFunc = do
            args <- getArgs
            route <- getRoute $ if length args > 0 then head args else "activity.gpx"
            putStrLn $ "Total distance " ++ (show $ totalDistance route)
            putStrLn $ "Total time " ++ (showTime $ totalTime route)
            putStrLn $ "Best 1k " ++ (showSection $ fastestNk 1 (trackPoints route))
            putStrLn $ "Best 5k " ++ (showSection $ fastestNk 5 (trackPoints route))
            putStrLn $ "Best 10k " ++ (showSection $ fastestNk 10 (trackPoints route))
            putStrLn $ "Best mile " ++ (showSection $ fastestNk 1.609344 (trackPoints route))
            putStrLn "Pace chart v0.1b"
            mapM putStrLn $ paceChart route 80 10
            return ()
