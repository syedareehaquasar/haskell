module Prime (nth) where

firstFactorAfter :: Int -> Int -> Int
firstFactorAfter k n | rem n k == 0 = k
firstFactorAfter k n | k * k > n = n
firstFactorAfter k n | otherwise = firstFactorAfter (k + 1) n

firstFactor :: Int -> Int
firstFactor n = firstFactorAfter 2 n

isPrime :: Int -> Bool
isPrime n | n < 2 = False
isPrime n | elem n [2, 3, 5, 7] = True
isPrime n | even n = False
isPrime n | otherwise = firstFactor n == n

nthPrime :: Int -> Int
nthPrime n | n <= 0 = -1
nthPrime n | otherwise = last $ take n $ filter isPrime [2..]

nthPrime1 :: Int -> Int
nthPrime1 n | n <= 0 = -1
nthPrime1 n | otherwise = (filter isPrime [2..]) !! (n - 1)

nth :: Int -> Maybe Integer
nth n | n <= 0 = Nothing
nth n | otherwise = Just $ toInteger $ (filter isPrime [2..]) !! (n - 1)