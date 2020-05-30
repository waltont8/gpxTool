{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Gpx
    ( getRoute
     ,TrackPoint (..)
     ,Route (..)
     ,Section
     ,secondsSince
     ,fastestNk
    ) where

import Text.XML.HXT.Core  -- The XML parser
import Data.Time          -- For the UTCTime type
import Data.Time.ISO8601  -- Parse the datetime format in gpx files

import Debug.Trace as D

-- Make the XML code a bit more readable
parseXML doc = readString [ withValidate no
                          , withRemoveWS yes
                          ] doc
atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText
textAtTag tag = atTag tag >>> text

-- The data structure holding details about points on the run
data TrackPoint = TrackPoint
  { longitude
   ,latitude
   ,elevation :: Float
   ,time :: UTCTime
  } deriving (Eq, Show)

type Distance = Float

type Section = (Distance, NominalDiffTime)

makeSection :: TrackPoint -> TrackPoint -> Section
makeSection a b = ((pointDistance a b), (secondsSince (time a) (time b) ))

runfinity = (1000, 3600000)

lessTime :: Section -> Section -> Bool
lessTime (_,t1) (_,t2) = t1 < t2

lessDistance :: Section -> Section -> Bool
lessDistance (d1,_) (d2,_) = d1 < d2

data Route = Route
  {
      trackPoints :: [TrackPoint]
      ,totalDistance :: Distance
      ,totalTime :: NominalDiffTime
  }

getTrackpoints = atTag "trkpt" >>>
  proc x -> do
    lon <- getAttrValue "lon"   -< x
    lat <- getAttrValue "lat"   -< x
    ele <- textAtTag "ele"   -< x
    tim <- textAtTag "time"  -< x
    returnA -< TrackPoint
      { longitude  = read lon,
        latitude   = read lat,
        elevation  = read ele,
        time       = badParseISO8601 tim }

badParseISO8601 :: String -> UTCTime
badParseISO8601 s = case d of
                      Just v -> v
                      Nothing -> error "Bailed parsing dates"
                     where
                       d = parseISO8601 s

getGPX filename = do
  doc    <- readFile filename
  xml    <- return $ parseXML doc
  result <- runX (xml >>> getTrackpoints)
  case result of
    []  -> error "Unable to parse gpx data."
    otherwise -> return result

secondsSince :: UTCTime -> UTCTime -> NominalDiffTime
secondsSince t0 t1 =  diffUTCTime t1 t0

{------------------------------------------------------------------------------------------------------}
{- https://rosettacode.org/wiki/Haversine_formula?source=post_page---------------------------#Haskell -}
-- The haversine of an angle.
haversine :: Float -> Float
haversine = (^ 2) . sin . (/ 2)
 
-- The approximate distance, in kilometers, between two points on Earth.
-- The latitude and longtitude are assumed to be in degrees.
earthDist :: (Float, Float) -> (Float, Float) -> Float
earthDist = distDeg 6371
  where
    distDeg radius p1 p2 = distRad radius (deg2rad p1) (deg2rad p2)
    distRad radius (lat1, lng1) (lat2, lng2) =
      (2 * radius) *
      asin
        (min
           1.0
           (sqrt $
            haversine (lat2 - lat1) +
            ((cos lat1 * cos lat2) * haversine (lng2 - lng1))))
    deg2rad = d2r *** d2r
      where
        d2r = (/ 180) . (pi *)
{------------------------------------------------------------------------------------------------------}
pointDistance :: TrackPoint -> TrackPoint -> Distance
pointDistance a b = earthDist (latitude a, longitude a) (latitude b, longitude b)

-- Implement a sliding window over route sections using recursion
findNkInner :: Float -> [Section] -> Section -> Section -> [Section] -> Section
findNkInner _ [] _ best _ = best
findNkInner n secs (totalDist, totalTime) best store = if totalDist < n
                                                        -- grab something from secs and move it to store updating everything
                                                         then findNkInner n 
                                                                          (tail secs) 
                                                                          (addToTotal (totalDist, totalTime) (head secs)) 
                                                                          best
                                                                          ((head secs):store)
                                                       else  --remove something from the store and throw it away, removing it from everything
                                                           findNkInner n 
                                                                      secs 
                                                                      ( removeFromTotal (totalDist, totalTime) (last store) )
                                                                      (theBest best (totalDist, totalTime)) 
                                                                      (init store)
                                                          where
                                                            addToTotal (d,t) (sd, st) = (d+sd, t+st)
                                                            theBest (d,t) (sd, st) = if (t<st) then (d,t) else (sd,st)
                                                            removeFromTotal (d, t) (rd, rt) = (d-rd, t-rt)

-- Work on the sections between points
fastestNk :: Float -> [TrackPoint] -> Section
fastestNk n tp = findNkInner n sections (0,0) runfinity []
                  where
                    sections = zipWith makeSection tp (tail tp)


routeDistance :: [TrackPoint] -> Distance
routeDistance (h:n:xs) = earthDist (latitude h, longitude h) (latitude n, longitude n) + routeDistance (n:xs)
routeDistance (h:[]) = 0

routeTime :: [TrackPoint] -> NominalDiffTime
routeTime p = secondsSince (time $ head p) (time $ last p)

getRoute :: String -> IO Route
getRoute fileName = do
            pts <- getGPX fileName
            return $ Route pts (routeDistance pts) (routeTime pts)
            