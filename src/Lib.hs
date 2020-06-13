module Lib
    ( someFunc
    ) where

import System.Environment

import Gpx

interestingDistances = [1.0, 5.0, 10.0, 1.609344, 16.09344, 21.0975, 42.195]

someFunc = do
            args <- getArgs
            route <- getRoute $ if length args > 0 then head args else "activity.gpx"
            putStrLn $ "Total distance " ++ (show $ totalDistance route)
            putStrLn $ "Total time " ++ (showTime $ totalTime route)
            mapM putStrLn $ map (\x -> if ((totalDistance route) > x) then 
                                          "Fastest " ++ (showDistance x) ++ " " ++ (showSection $ fastestNk x route) 
                                      else "Extrapolate " ++ (showDistance x) ++ " " ++ (showTime $ extrapolate route x)
                                ) interestingDistances
            putStrLn "Pace chart v0.1b"
            mapM putStrLn $ paceChart route 80 10
            return ()
