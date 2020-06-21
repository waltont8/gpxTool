{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Gpx
    ( getRoute
     ,TrackPoint (..)
     ,Route (..)
     ,Section
     ,secondsSince
     ,fastestNk
     ,paceChart
     ,showSection
     ,showTime
     ,extrapolate
     ,showDistance
     ,buildAMap
     ,angle
    ) where

import Text.XML.HXT.Core  -- The XML parser
import Data.Time          -- For the UTCTime type
import Data.Time.ISO8601  -- Parse the datetime format in gpx files
import System.Time.Utils (renderSecs)
import Data.List.Split
import Debug.Trace as D
import Data.List
import Data.Map as Map (fromList, member, lookup)

-- Make the XML code a bit more readable
parseXML = readString [ withValidate no
                          , withRemoveWS yes
                          ]

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

data Route = Route
  {
      trackPoints :: [TrackPoint]
      ,totalDistance :: Distance
      ,totalTime :: NominalDiffTime
  } deriving (Eq, Show)

type Distance = Float
type Section = (Distance, NominalDiffTime)
type Pace = NominalDiffTime

makeSection :: TrackPoint -> TrackPoint -> Section
makeSection a b = ((pointDistance a b), (secondsSince (time a) (time b) ))

runfinity = (1000, 3600000)

lessTime :: Section -> Section -> Bool
lessTime (_,t1) (_,t2) = t1 < t2

lessDistance :: Section -> Section -> Bool
lessDistance (d1,_) (d2,_) = d1 < d2

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
  let xml = parseXML doc
  result <- runX (xml >>> getTrackpoints)
  case result of
    []  -> error "Unable to parse gpx data."
    _ -> return result

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
findNkInner n secs (totalDist, totalTime) best store
           | totalDist < n = findNkInner n
                                         (tail secs)
                                         (addToTotal (totalDist, totalTime) (head secs))
                                         best
                                         ((head secs):store)
           | otherwise =     findNkInner n
                                         secs
                                         ( removeFromTotal (totalDist, totalTime) (last store) )
                                         (theBest best (totalDist, totalTime))
                                         (init store)
              where
                addToTotal (d,t) (sd, st) = (d+sd, t+st)
                theBest (d,t) (sd, st) = if (t<st) then (d,t) else (sd,st)
                removeFromTotal (d, t) (rd, rt) = (d-rd, t-rt)

-- Work on the sections between points
fastestNk :: Float -> Route -> Section
fastestNk n (Route tp _ _) = findNkInner n sections (0,0) runfinity []
                  where
                    sections = zipWith makeSection tp (tail tp)

extrapolate :: Route -> Distance -> NominalDiffTime
extrapolate (Route _ td tt) d = realToFrac $ ((fromIntegral (round tt)) / td) * d


routeDistance :: [TrackPoint] -> Distance
routeDistance (h:n:xs) = earthDist (latitude h, longitude h) (latitude n, longitude n) + routeDistance (n:xs)
routeDistance (h:[]) = 0

routeTime :: [TrackPoint] -> NominalDiffTime
routeTime p = secondsSince (time $ head p) (time $ last p)

getRoute :: String -> IO Route
getRoute fileName = do
            pts <- getGPX fileName
            return $ Route pts (routeDistance pts) (routeTime pts)

paceFromSections :: [Section] -> Pace
paceFromSections s = (realToFrac (1.0/sumDist)) * sumTime
                      where
                        (sumDist, sumTime) = foldl (\(sd, st) (d,t) -> (sd+d,st+t)) (0,0) s

splitSectionAt :: [Section] -> Distance -> ([Section], [Section])
splitSectionAt sections = splitAtInner sections []
                where
                  splitAtInner :: [Section] -> [Section] -> Distance -> ([Section], [Section])
                  splitAtInner [] before d = (before, [])
                  splitAtInner s@((sd,st):xs) before d
                              | d == 0 = (before, s)
                              | d < sd = splitAtInner ((splitAfter sd st d):xs) ((splitBefore sd st d):before) 0
                              | otherwise = splitAtInner xs ((sd,st):before) (d - sd)
                                where
                                  splitBefore dd tt d = (d, tt * (realToFrac (d/dd)))
                                  splitAfter  dd tt d = (dd-d, tt - (tt * (realToFrac (d/dd))))

chunkRoute :: Route -> Distance -> [[Section]]
chunkRoute r = chunkSections sections
                  where
                    sections = zipWith makeSection (trackPoints r) (tail (trackPoints r))
                    chunkSections [] _ = []
                    chunkSections cSecs cD = (fst $ splitSectionAt cSecs cD) : (chunkSections (snd $ splitSectionAt cSecs cD)) cD

paceChunks :: Route -> Distance -> [NominalDiffTime]
paceChunks r d = map paceFromSections sections
                  where
                    sections = chunkRoute r d

paceChart :: Route -> Int -> Int -> [String]
paceChart r w h = zipWith (++) (reverse . transpose $ map (\p -> concat $ (replicate p "#") ++ (replicate (h-p) " ")) heights) times
                  where
                    paces = paceChunks r (totalDistance r / (fromIntegral w))
                    fastest = minimum paces
                    slowest = maximum paces
                    paceDivide = (fastest-slowest)/(fromIntegral h)
                    heights = map (\p -> round ((p-slowest)/paceDivide)) paces
                    times = map ((" " ++) . showTime) (timeSteps slowest fastest 10)
                    timeSteps :: NominalDiffTime -> NominalDiffTime -> Int -> [NominalDiffTime]
                    timeSteps f t s = timeStepsInner f t s s
                                        where
                                          timeStepsInner :: NominalDiffTime -> NominalDiffTime -> Int -> Int -> [NominalDiffTime]
                                          timeStepsInner f t s c
                                                        | c == 0 = []
                                                        | otherwise = (f + (((t-f)/realToFrac s)*realToFrac c)) : timeStepsInner f t s (c-1)

buildAMap :: Route -> Int -> Int -> String
buildAMap (Route tp td tt) w h = "+" ++ replicate w '-' ++ "+\n|" ++ mapBuilder 0 h ++ "+" ++ replicate w '-' ++ "+\n"
            where
              sections = zip tp (tail tp)
              mostWest = minimum (map (longitude . fst) sections)
              mostEast = maximum (map (longitude . fst) sections)
              mostNorth = maximum (map (latitude . fst) sections)
              mostSouth = minimum (map (latitude . fst) sections)
              maxWidth = mostEast - mostWest
              maxHeight = mostNorth - mostSouth
              stepWidth  = maxWidth / (fromIntegral w)
              stepHeight = maxHeight / (fromIntegral h)
              pointToArray (pt, pt2) = ((round ((x-mostWest)/stepWidth)) + ( round ((y-mostSouth)/stepHeight) ) * w, angleToChar (angle pt pt2)) -- return dictionary key and character
                          where
                            x = longitude pt
                            y = latitude pt
              allPoints = Map.fromList $ map pointToArray sections -- Add these to a dictionary so it can be rapidly queried whilst building the map
              mapBuilder x y
                        | x < w = (if (Map.member (x+y*w) allPoints) then (whatever $ Map.lookup (x+y*w) allPoints) else ' ') : (mapBuilder (x+1) y)
                        | x == w && y > 0 = "|\n|" ++ (mapBuilder 0 (y-1))
                        | otherwise = "|\n"

whatever :: Maybe a -> a
whatever (Just a) = a
whatever Nothing = error "whatever!"

angle :: TrackPoint -> TrackPoint -> Float
angle a b = (atan2 (x2-x1) (y2-y1)) * (180.0/3.14159) + (if x2 < x1 then 360.0 else 0.0)
              where
                x1 = longitude a
                x2 = longitude b
                y1 = latitude a
                y2 = latitude b

                -- Display functions

angleToChar :: Float -> Char
angleToChar f
            | f > 337.5 || f <= 22.5 = '|'
            | f > 157.5 && f <= 202.5 = '|'
            | f > 67.5 && f <= 112.5 = '-'
            | f > 247.5 && f <= 292.5 = '-'
            | f > 22.5 && f <= 67.5 = '/'
            | f > 202.5 && f <= 247.5 = '/'
            | f > 112.5 && f <= 157.5 = '\\'
            | f > 292.5 && f <= 337.5 = '\\'
            | otherwise = '#'

showTime = renderSecs . round :: NominalDiffTime -> String

showSection :: Section -> String
showSection (d,t) = "(" ++ (show d) ++ "Km, " ++(showTime t) ++ ")"

showDistance x
            | x == 1.609344 = "Mile"
            | x == 16.09344 = "10 miles"
            | x == 21.0975 = "Half Marathon"
            | x == 42.195 = "Marathon"
            | otherwise = (show x) ++ "k"