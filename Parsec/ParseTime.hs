module JTPrettyTime.Parsec.ParseTime
( parseCombinedDelimiter
, parseTime
) where

import Text.Parsec
import Text.Parsec.String

import JTPrettyTime.Parsec.ParseUtils

parseCombinedDelimiter :: Parser Integer
parseCombinedDelimiter = (char 'T' <|> char ' ') >> return 0

parseTime :: Parser Integer
parseTime = do
  h <- parseDigits 2
  m <- parseMinutes -- Returns seconds
  return (h*60*60+m)

parseMinutes :: Parser Integer
parseMinutes = pBasic <|> pExt <|> return 0
  where
    -- hhmm[ss.sss]
    pBasic = do
      m <- parseDigits 2
      s <- parseSeconds <|> return 0
      return (m*60+s)
    -- hh:mm[:ss.sss]
    pExt = do
      _ <- char ':'
      m <- parseDigits 2
      s <- ifPresentThen ':' parseSeconds
      return (m*60+s)

parseSeconds :: Parser Integer
parseSeconds = do
  s <- parseDigits 2
  _ <- ifPresentThen '.' $ parseDigits 3
  return s
