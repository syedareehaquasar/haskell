count c = length . filter (== c)

dCount :: [Char] -> Bool
dCount walk = and ([x!!0 == x!!1] ++ [x!!2 == x!!3])
                  where x = [(count c walk) | c <- ['n', 's', 'e', 'w']]

isValidWalk :: [Char] -> Bool
-- isValidWalk walk | isInfinite
isValidWalk walk = length (take 11 walk) == 10 && dCount walk

main = do
    print $ isValidWalk ['n','s','n','s','n','s','n','s','n','s']
    print $ isValidWalk ['n','s','n','s','n','s','n','s','n','n']
    print $ isValidWalk ['n','s']
    -- print $ isValidWalk (repeat 'n')