notEndsInZero :: Int -> Bool
notEndsInZero n = rem n 10 /= 0

countZeros :: Int -> Int
countZeros n | notEndsInZero n = 0
countZeros n | otherwise      = 1 + (countZeros $ quot n 10)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1] , x <- combinations (n-1) (drop (i+1) xs) ]

productsOfSubsets :: [Int] -> Int -> [Int]
productsOfSubsets array k =  [product s | s <- combinations k array]

codelab1 :: [Int] -> Int -> Int 
codelab1 array k = maximum $ map countZeros $ productsOfSubsets array k

main = print $ codelab1 [50, 4, 20] 2