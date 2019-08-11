module JTPrettyTime.Parsec.ParseIso8601
( parseIso8601
, parseIso8601Strict
, iso8601Parser
) where

import Text.Parsec
import Text.Parsec.String

import JTPrettyTime.Parsec.ParseDate
import JTPrettyTime.Parsec.ParseTime
import JTPrettyTime.Parsec.ParseTimezone

-- Use this to read the string as far as possible, best effort style.
-- WARNING: This means that a malformed input might be partially read
-- and partially ignored.
parseIso8601 :: String -> Either String Int
parseIso8601 = sandboxParse iso8601Parser

-- Use this to make sure that ALL input is parsed.
-- WARNING: Trailing newlines trip this up.
parseIso8601Strict :: String -> Either String Int
parseIso8601Strict = sandboxParse strictParser
  where
    strictParser = do
      v <- iso8601Parser
      eof
      return v

-- You can also use the parser directly
iso8601Parser :: Parser Int
iso8601Parser = try pFull <|> parseReducedDate
  where
    pFull = chainedParse [
      parseDateTriple,
      parseCombinedDelimiter,
      parseTime,
      parseTZ]

sandboxParse :: Parser Int -> String -> Either String Int
sandboxParse p input = normalize output
  where
    output = parse p failMsg input
    failMsg = "Failed to parse ISO8601 from string \""++input++"\""
    normalize (Left err) = Left $ show err
    normalize (Right v) = Right v

chainedParse :: [Parser Int] -> Parser Int
chainedParse [] = return 0
chainedParse (x:xs) = do
  v <- x
  vTail <- chainedParse xs <|> return 0
  return (v+vTail)

