import JTPrettyTime.Util

-- Compile like this
-- ghc example3.hs -Wall -Werror --make -i.:../.. \
--     -outputdir obj/ -o example3

main :: IO ()
main = do
  b <- isAnniversary 123456  -- compared to today
  print b
 
