import Data.Char

numToDigits :: Int -> [Int]
numToDigits = map digitToInt . show


isArmstrong :: Int -> Bool
isArmstrong n = n == sum [x ^ p| x <- d] where d = numToDigits n
                                               p = length d

makeArmstrong :: Int -> Int -> [Int]
makeArmstrong  a b = filter isArmstrong [a..b]


main = do
    print $ makeArmstrong 100 1000
