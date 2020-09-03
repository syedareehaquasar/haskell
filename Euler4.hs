isPalindrome :: Int -> Bool
isPalindrome x = show x == (reverse $ show x)

palindromicProduct :: Int -> Int -> [Int]
palindromicProduct start limit = [ab | a <- [start..limit], b <- [a..limit], let ab = a * b, isPalindrome ab]

euler4 :: Int -> Int -> Int
euler4 start limit = maximum $ palindromicProducts start limit

main = do
  print $ euler4 100 999
