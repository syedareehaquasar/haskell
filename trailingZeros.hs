factorial :: Int -> Int
factorial x = product $ [x, x-1..1]

fpl :: [Int]
fpl = zipWith (^) (repeat 5) [1..]

zeros :: Int -> Int
zeros 0 = 0
zeros n = sum $ [div n x | x <- (takeWhile (<=n) fpl)]

main = do
    print $ factorial 12
    print $ zeros 12
    print $ factorial 100
    print $ zeros 100
    n <- getLine
    let x = read n :: Int
    print $ factorial x
    print $ zeros x