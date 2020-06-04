nextCollatz :: Int -> Int
nextCollatz n = if even n then quot n 2 else 3 * n + 1


collatz :: Int -> [Int]
collatz 4 = [4, 2, 1]
collatz n = n : (collatz $ nextCollatz n)
-- collatz n = [n] ++ (collatz $ nextCollatz n)

main = do
    print $ collatz 7
