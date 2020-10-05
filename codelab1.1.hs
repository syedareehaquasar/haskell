notEndsInZero :: Int -> Bool
notEndsInZero n = rem n 10 /= 0

-- select :: [Int] -> Int -> Int -> [Int]
-- select array index1 index2 = drop index1 $ take index2 array

trailingInZeros' :: Int -> Int
trailingInZeros' n | notEndInZero n = 0
trailingInZeros' n | otherwise      = 1 + (trailingInZeros' $ quot n 10)

trailingInZeros :: [Int] -> Int
trailingInZeros nums = trailingzeros' $ product nums

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1] , x <- combinations (n-1) (drop (i+1) xs) ]

codelab1 :: [Int] -> Int -> Int 
codelab1 array k = maximum $ trailingInZeros $ [pick_from !! i | i <- [0..length (pick_from) - 1], where pick_from = combinations k array]

-- atoi :: String -> Int
-- atoi s = read s :: Int

main = do
    -- array <- getLine
    -- n <- getLine
    print $ codelab1 [1, 2, 3, 4] 2