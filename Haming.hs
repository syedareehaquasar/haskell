distance :: String -> String -> Maybe Int
distance x y | length x /= length y = Nothing
             | otherwise = Just . length . filter not $ zipWith (==) x y
         
      
  
distances :: String -> String -> Int
distances x y | length x \= length y = -1
              | otherwise = length $ filter (==True) $ zipwith (/=) x y
              
 
 main = do
  print $ distance $ "GAGCCTACTAACGGGAT" "CATCGTAATGACGGCCT"
  print $ distances $ "GAGCCTACTAACGGGAT" "CATCGTAATGACGGCCT"
