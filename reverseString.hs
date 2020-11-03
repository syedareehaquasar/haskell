revStr :: String -> String
-- revStr s = unwords(reverse(words(s)))
revStr = unwords . reverse . words

main = print (revStr("you can't lose can you"))