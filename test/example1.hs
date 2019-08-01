import JTPrettyTime.Parsec.ParseIso8601

-- Compile like this
-- ghc example1.hs -Wall -Werror --make -i.:../.. \
--     -outputdir obj/ -o example1

-- Test with
-- date -d @$(./example1 < example1_input.txt)
-- NOTE: This doesn't work with parseIso8601Strict because each line
-- has a trailing Line Feed (\n). This is required by UNIX definition
-- of a line. The line feed is not read by the parser and the
-- function fails accordingly.

-- Get a unix timestamp from bash
-- date -d "2019-03-10T12:36:49-02:00" +%s

play :: String -> IO ()
play inp = case parseIso8601 inp of
             { Left err -> print err
             ; Right ans -> print ans
             }

main :: IO ()
main = do
    contents <- getContents
    play contents
 
