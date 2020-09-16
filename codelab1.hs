notEndsInZero :: Int -> Bool
notEndsInZero n = rem n 10 /= 0

codelab1 :: [Int] -> Int -> Int
codelab1 array k = maximum $ _

select :: [Int] -> Int -> Int -> [Int]
select array index1 index2 = drop index1 $ take index2 array

select' :: [Int] -> Int -> [[Int]]

trailingInZeros :: [Int] -> Int
trailingInZeros nums = trailingzeros' $ product nums

trailingInZeros' :: Int -> Int
trailingInZeros' n | notEndInZero n = 0
trailingInZeros' n | otherwise      = 1 + (trailingInZeros' $ quot n 10)
