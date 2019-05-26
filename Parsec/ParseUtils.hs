module JTPrettyTime.Parsec.ParseUtils
( parseDigits
, ifPresentThen
) where

import Text.Parsec
import Text.Parsec.String

parseDigits :: Int -> Parser Int
parseDigits n = read <$> count n digit

-- If the character can be parsed, use the parser
ifPresentThen :: Char -> Parser Int -> Parser Int
ifPresentThen c p = (char c >> p) <|> return 0
 
