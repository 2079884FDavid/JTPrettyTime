import ParseIso8601

-- Test with
-- date -d @$(./main < input.txt)

-- Reverse
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
 
