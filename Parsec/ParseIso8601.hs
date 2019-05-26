module JTPrettyTime.Parsec.ParseIso8601
( parseIso8601
) where

import JTPrettyTime.Parsec.ParseDate
import JTPrettyTime.Parsec.ParseTime
import JTPrettyTime.Parsec.ParseTimezone
import Text.Parsec
import Text.Parsec.String

parseIso8601 :: String -> Either ParseError Int
parseIso8601 input = parse pChoice st input
  where
    st = "Failed to parse ISO8601 from string \""++input++"\""
    pChoice = try pFull <|> parseReducedDate
    pFull = chainedParse [
      parseDateTriple,
      parseCombinedDelimiter,
      parseTime,
      parseTZ]

chainedParse :: [Parser Int] -> Parser Int
chainedParse [] = return 0
chainedParse (x:xs) = do
  v <- x
  vTail <- chainedParse xs <|> return 0
  return (v+vTail)

