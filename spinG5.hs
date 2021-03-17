spin :: String -> String
spin s | length(s) >= 5 = reverse(s)
       | otherwise = s

spinWords :: String -> String
spinWords str = unwords $ map spin $ words str

main = do
    print $ spinWords( "Hey fellow warriors" )
    print $ spinWords( "This is a test")
    print $ spinWords( "This is another test" )
    
    n <- getLine
    print $ spinWords( show n)