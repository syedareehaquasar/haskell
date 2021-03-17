import Data.Char(isDigit)

phnNumCleaner :: String -> String
-- phnNumCleaner s = if length(clean s) > 10 then tail(clean s) else clean s
phnNumCleaner s | length(xs) > 10 = tail xs
                | otherwise = xs
                where xs = clean s

clean :: String -> String
clean s = filter isDigit s

main = do
    print $ phnNumCleaner "+1 (613)-995-0253"
    print $ phnNumCleaner "987-045-557-2"