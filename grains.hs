import Data.Maybe

square :: Integer -> Maybe Integer
square n | n < 1 || n > 64 =  Nothing
         | otherwise = Just $ 2 ^ (n - 1) 

total :: Integer
total = maybe 0 (\n -> 2*n - 1) $ square 64

total1 :: Num a => a
total1 = sum(map square x) where x = [1,2..64]

main = do
  print $ square(12)
  print $ total
  putStrLn ("Enter the grains you want to know of a square")
  n <- getLine
  print(n)
