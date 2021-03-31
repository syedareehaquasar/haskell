square :: Integer -> Maybe Integer
square n | n < 1 || n > 64 =  Nothing
         | otherwise = Just $ 2 ^ (n - 1) 

total :: Integer
total = maybe 0 (\n -> 2*n - 1) $ square 64

main = do
  print $ square(12)
  print $ total
  print $ "Enter the grains you want to know of a square"
  n <- getLine
  print(n)
