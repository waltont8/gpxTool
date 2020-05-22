{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Gpx
    ( getTrackPoints
     ,TrackPoint (..)
     ,secondsSince
    ) where

import Text.XML.HXT.Core  -- The XML parser
import Data.Time          -- For the UTCTime type
import Data.Time.ISO8601  -- Parse the datetime format in gpx files

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

secondsSince :: UTCTime -> UTCTime -> (Integer, Int)
secondsSince t0 t1 = (i, round $ d * 1000)
   where (i, d) = properFraction $ diffUTCTime t1 t0

getTrackPoints :: String -> IO [TrackPoint]
getTrackPoints fileName = do
            pts <- getGPX fileName
            return pts
