module JTPrettyTime.Parsec.ParseIso8601
( parseIso8601
, isIso8601StrictParseable
, iso8601Parser
, parseIso8601Forced
, parseIso8601Strict
) where

import Data.Either
import Text.Parsec
import Text.Parsec.String

import JTPrettyTime.Parsec.ParseDate
import JTPrettyTime.Parsec.ParseTime
import JTPrettyTime.Parsec.ParseTimezone

-- Use this to read the string as far as possible, best effort style.
-- WARNING: This means that a malformed input might be partially read
-- and partially ignored.
parseIso8601 :: String -> Either String Integer
parseIso8601 = sandboxParse iso8601Parser

-- Use this to make sure that ALL input is parsed.
-- WARNING: Trailing newlines trip this up.
parseIso8601Strict :: String -> Either String Integer
parseIso8601Strict = sandboxParse strictParser
  where
    strictParser = do
      v <- iso8601Parser
      eof
      return v

-- Use this if you are SURE that the input cannot be malformed.
-- WARNING: There is no safety net here. If you provide wrong
-- input, your program will go into an undefined state (returns
-- unix-timestamp=0) and things will go badly.
-- DO NOT TRUST USER INPUT!
parseIso8601Forced :: String -> Integer
parseIso8601Forced s = case parseIso8601 s of
  -- fromRight is only available in newer GHC versions
  (Left _ ) -> 0
  (Right t) -> t

-- Check if parseIso8601Strict would succeed on input
isIso8601StrictParseable :: String -> Bool
isIso8601StrictParseable = isRight . parseIso8601Strict

-- You can also use the parser directly
iso8601Parser :: Parser Integer
iso8601Parser = try pFull <|> parseReducedDate
  where
    pFull = chainedParse [
      parseDateTriple,
      parseCombinedDelimiter,
      parseTime,
      parseTZ]

sandboxParse :: Parser Integer -> String -> Either String Integer
sandboxParse p input = normalize output
  where
    output = parse p failMsg input
    failMsg = "Failed to parse ISO8601 from string \""++input++"\""
    normalize (Left err) = Left $ show err
    normalize (Right v) = Right v

chainedParse :: [Parser Integer] -> Parser Integer
chainedParse [] = return 0
chainedParse (x:xs) = do
  v <- x
  vTail <- chainedParse xs <|> return 0
  return (v+vTail)

