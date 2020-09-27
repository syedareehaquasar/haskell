--isPrime n = [x | x <- [1..n-1], mod n x == 0] == [1]

isPrime :: Int -> Bool
-- isPrime n | elem n [0, 1] = False
isPrime n | n < 2               = False
isPrime n | elem n [2, 3, 5, 7] = True
isPrime n | even n              = False
isPrime n | otherwise           = and [rem n f /= 0 | f <- [3,5..limit]] where limit = floor $ sqrt $ fromInteger n

main = do
    n <- getLine
    print $ isPrime (read n :: Int)