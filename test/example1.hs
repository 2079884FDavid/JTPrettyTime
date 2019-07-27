import JTPrettyTime.Parsec.ParseIso8601

-- Compile like this
-- ghc example1.hs -Wall -Werror --make -i.:../.. -outputdir obj/ -o example1

-- Test with
-- date -d @$(./example1 < example1_input.txt)

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
 
