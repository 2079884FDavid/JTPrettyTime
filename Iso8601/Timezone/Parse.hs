module JTPrettyTime.Iso8601.Timezone.Parse
( parse
) where

import Data.Maybe
import Text.Read
import qualified Data.Map as Map

designatorLookupMap :: Map.Map Char Integer
designatorLookupMap = Map.fromList $ map (\(z, h) -> (z, h * 3600)) [
  ('A', 1),
  ('B', 2),
  ('C', 3),
  ('D', 4),
  ('E', 5),
  ('F', 6),
  ('G', 7),
  ('H', 8),
  ('I', 9),
  ('K', 10),
  ('L', 11),
  ('M', 12),
  ('N', (-1)),
  ('O', (-2)),
  ('P', (-3)),
  ('Q', (-4)),
  ('R', (-5)),
  ('S', (-6)),
  ('T', (-7)),
  ('U', (-8)),
  ('V', (-9)),
  ('W', (-10)),
  ('X', (-11)),
  ('Y', (-12)),
  ('Z', 0)]

lookupDesignator :: String -> Maybe Integer
lookupDesignator s = do
  t <- listToMaybe s --Get the first element if it exists 
  Map.lookup t designatorLookupMap

readTwoDigits :: String -> Maybe Integer
readTwoDigits (x:y:_) = readMaybe [x,y]
readTwoDigits _ = Nothing

ifMatchRun :: String -> Char -> (String -> Maybe Integer) -> Maybe Integer
ifMatchRun (x:xs) c f = if x == c 
  then f xs
  else Nothing
ifMatchRun [] _ _ = Nothing

readOffset :: String -> Maybe Integer
readOffset s = do
  h <- readTwoDigits s
  return (h*3600+m*60)
  where
    m = readOffsetMinutes (drop 2 s)

matchOrDefault :: [Maybe a] -> a -> a
matchOrDefault xs d = f (catMaybes xs)
  where
    f (x:_) = x
    f [] = d

--The minute offset can return a default because the ISO
--standard requires only an hour. +02:FOOBAR is valid
readOffsetMinutes :: String -> Integer
readOffsetMinutes s = matchOrDefault [m, cm] 0
  where
    m = readTwoDigits s
    cm = ifMatchRun s ':' readTwoDigits

parse :: String -> Maybe Integer
parse s = listToMaybe $ catMaybes matches
  where
    designator = lookupDesignator s
    positive = ifMatchRun s '+' readOffset
    negative = ifMatchRun s '-' negateOffset
    matches = [designator, positive, negative]
    negateOffset s' = do
      r <- readOffset s'
      return (r*(-1))
