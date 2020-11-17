divisiblePairs ::  Int -> [Int] -> Int
divisiblePairs k arr = length [(i, j) | i <- [0..length arr - 2],
                                          j <- [0..length arr - 1],
                                          (arr !! i + arr !! j) `mod` k == 0,
                                          i < j]

isMultiple :: Int -> Int -> Bool
isMultiple k x = rem x k == 0

divisibleSumPairs :: [Int] -> Int -> Int
divisibleSumPairs arr k = length $ filter (isMultiple k) [(head arr) + t | t <- tail arr]

countDivSumPairs :: [Int] -> Int -> Int
countDivSumPairs x:xs k = length $ filter (isMultiple k) [x + t | t <- xs]

countAllDivSumPairs :: [Int] -> Int -> Int
countAllDivSumPairs [] k = 0
countAllDivSumPairs [x] k = 0
countAllDivSumPairs arr k = divisibleSumPairs arr k + (countAllDivSumPairs (tail arr) k)

countAllDivSumPairs 

main = do
print $ divisiblePairs 3 [1, 3, 2, 6, 1, 2]
print $ countAllDivSumPairs [1, 3, 2, 6, 1, 2] 3

