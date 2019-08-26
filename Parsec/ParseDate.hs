module JTPrettyTime.Parsec.ParseDate
( parseDateTriple
, parseReducedDate
) where

import Data.UnixTime
import Text.Parsec
import Text.Parsec.String
import Text.Printf
import qualified Data.ByteString.Char8 as C

import JTPrettyTime.Parsec.ParseUtils

-- year ::= digit digit digit digit
-- month ::= digit digit
-- day ::= digit digit
-- date ::= year
--        | year - month 
--        | year month 
--        | year - month - day 
--        | year month day

-- YYYY-MM-DD or YYYYMMDD
parseDateTriple :: Parser Integer
parseDateTriple = do
  y <- parseDigits 4
  (m, d) <- parseMonthDay
  return $ tripleToTimestamp (y, m, d)

-- -MM-DD or MMDD
parseMonthDay :: Parser (Integer, Integer)
parseMonthDay = dash <|> nodash
  where
    dash = do
      _ <- char '-'
      m <- parseDigits 2
      _ <- char '-'
      d <- parseDigits 2
      return (m, d)
    nodash = do
      m <- parseDigits 2
      d <- parseDigits 2
      return (m, d)

-- YYYY or YYYY-MM
parseReducedDate :: Parser Integer
parseReducedDate = do
  y <- parseDigits 4
  m <- m'
  return $ tripleToTimestamp (y, m, 1)
  where
    -- The month is optional
    m' = (char '-' >> parseDigits 2) <|> return 1

tripleToTimestamp :: (Integer, Integer, Integer) -> Integer
tripleToTimestamp (y,m,d) = 
  toInteger . fromEnum . utSeconds $ parseUnixTimeGMT f s
  where
    f = C.pack "%Y-%m-%d"
    s = C.pack $ printf "%d-%d-%d" y m d

