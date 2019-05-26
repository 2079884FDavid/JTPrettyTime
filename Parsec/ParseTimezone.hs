module JTPrettyTime.Parsec.ParseTimezone
( parseTZ
) where

import JTPrettyTime.Parsec.ParseUtils
import Text.Parsec
import Text.Parsec.String

parseTZ :: Parser Int
parseTZ = (char 'Z' >> return 0) <|> pOffset <|> return 0
  where
    pMinutes = (char ':' >> parseDigits 2) <|> parseDigits 2
    pOffset = do
      d <- (char '+' >> return 1) <|> (char '-' >> return (-1))
      h <- parseDigits 2
      m <- pMinutes <|> return 0
      return (d * (h*60*60 + m*60))
