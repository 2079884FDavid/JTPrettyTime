module JTPrettyTime.Iso8601
( parse
, parseTimezone
) where

import Data.Maybe
import qualified Data.Map as Map

parse :: String -> Integer
parse _ = 0

secsPerHour :: Integer
secsPerHour = 3600

militaryTZLookupMap :: Map.Map Char Integer
militaryTZLookupMap = Map.fromList $ map (\(z, h) -> (z, h * secsPerHour)) [
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

lookupTimeZone :: String -> Maybe Integer
lookupTimeZone s = do
  t <- listToMaybe s --Get the first element if it exists 
  Map.lookup t militaryTZLookupMap
   
parseTimezone :: String -> Integer
parseTimezone s = fromMaybe def $ listToMaybe matches
  where
    def = 0
    matches = catMaybes [lookupTimeZone s]
