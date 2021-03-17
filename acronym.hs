import Data.Char(toUpper, isUpper)

abbreviate :: String -> String
abbreviate xs = filter isUpper $ concat $ map upper1 $ words $ map clean xs

clean :: Char -> Char
clean '_' = ' '
clean '-' = ' ' 
clean ch = ch

upper1 :: String -> String
upper1 [] = []
upper1 (x:xs) = toUpper(x) : xs

--FOR SPLITTING
--splitOn -> data.List
--Words -> Prelude