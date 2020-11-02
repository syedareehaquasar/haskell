revStr :: String -> String
revStr s = unwords(reverse(words(s)))

main = print (revStr("you can't lose can you"))