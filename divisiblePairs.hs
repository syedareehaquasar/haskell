divisiblePairs :: Int -> Int -> [Int] -> Int
divisiblePairs n k arr = length [(i, j) | i <- [0..length arr - 2], j <- [0..length arr - 1], (arr !! i + arr !! j) `mod` k == 0, i < j]
