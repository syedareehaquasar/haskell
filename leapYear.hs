isLeap :: Int -> Bool
isLeap year = (mod year 400) == 0 || (mod year 4) == 0 && not ((mod year 100) == 0)

isLeap1 :: Int -> Bool
isLeap1 year | mod year 400 == 0 = True
             | mod year 100 == 0 = False
             | mod year 4 == 0 = True
             | otherwise = False

main  = do
    n <- getLine
    print $ isLeap( read (n))
    print $ isLeap1( read (n))