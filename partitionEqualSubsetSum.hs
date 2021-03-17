equalSubset :: [Int] -> Bool
equalSubset [] = False
equalSubset (x:xs) = if x == sum(xs) then True else equalSubset (xs ++ [x])

sublists :: [Int] -> [[Int]]
sublists = concatMap (tail . inits) . tails

main = do
    nums <- getLine
    print $ equalSubset(read $ nums)
