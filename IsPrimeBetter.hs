divides :: Int -> Int -> Bool
divides d n = rem n d == 0

firstFactorAfter :: Int -> Int -> Int
firstFactorAfter k n | divides k n = k
firstFactorAfter k n | k * k > n   = n
firstFactorAfter k n | otherwise   = firstFactorAfter (k + 1) n

firstFactor :: Int -> Int
firstFactor n = firstFactorAfter 2 n

isPrime :: Int -> Bool
isPrime n | n < 0     = error "Not a prime it's negative or zero"
isPrime n | n == 1    = False
isPrime n | otherwise = firstFactor n == n
