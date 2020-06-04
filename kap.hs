import Data.Char
import Data.List

num2Digits :: Int -> [Int]
num2Digits = map digitToInt . show

digits2Num :: [Int] -> Int
digits2Num = foldl1 (\x y -> 10 * x + y)

largest :: Int -> Int
largest = digits2Num . reverse . sort . num2Digits

smallest :: Int -> Int
smallest = digits2Num . sort . num2Digits

nextKaprekar :: Int -> Int
nextKaprekar n = largest n - smallest n

dropUntilRepeat :: Eq a =>  [a] -> [a]
dropUntilRepeat [] = []
dropUntilRepeat [n] = [n]
dropUntilRepeat (n:ns) = if n == head ns then [n] else n : (dropUntilRepeat ns)

-- dropUntilRepeat xs = if head xs == head (tail xs) then [head xs] else [head xs] ++ dropUntilRepeat (tail xs)

kaprekarSequence :: Int ->[Int]
kaprekarSequence = dropUntilRepeat . iterate nextKaprekar


main  = do
    start <- getLine
    print $ kaprekarSequence (read start :: Int)




