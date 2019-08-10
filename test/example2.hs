import JTPrettyTime.Local

-- Compile like this
-- ghc example2.hs -Wall -Werror --make -i.:../.. \
--     -outputdir obj/ -o example2

main :: IO ()
main = do
  s <- getCurrentLocalIso8601
  putStrLn s
