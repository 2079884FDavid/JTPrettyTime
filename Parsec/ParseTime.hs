module JTPrettyTime.Parsec.ParseTime
( parseCombinedDelimiter
, parseTime
) where

import JTPrettyTime.Parsec.ParseUtils
import Text.Parsec
import Text.Parsec.String

parseCombinedDelimiter :: Parser Int
parseCombinedDelimiter = char 'T' >> return 0

parseTime :: Parser Int
parseTime = do
  h <- parseDigits 2
  m <- parseMinutes -- Returns seconds
  return (h*60*60+m)

parseMinutes :: Parser Int
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

parseSeconds :: Parser Int
parseSeconds = do
  s <- parseDigits 2
  _ <- ifPresentThen '.' $ parseDigits 3
  return s
