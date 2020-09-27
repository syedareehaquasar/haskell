notEndsInZero :: Int -> Bool
notEndsInZero n = rem n 10 /= 0

select :: [Int] -> Int -> Int -> [Int]
select array index1 index2 = drop index1 $ take index2 array

-- select' :: [Int] -> Int -> [[Int]]

trailingInZeros' :: Int -> Int
trailingInZeros' n | notEndInZero n = 0
trailingInZeros' n | otherwise      = 1 + (trailingInZeros' $ quot n 10)

trailingInZeros :: [Int] -> Int
trailingInZeros nums = trailingzeros' $ product nums

codelab1 :: [Int] -> Int -> Int
codelab1 array k = maximum $ _

main = do
    array <- getLine
    n <- getLine
    print $ codelab1(read array :: [Int])