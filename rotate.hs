--rotating a list

rotate :: [a] -> Int -> [a]
rotate glist r = take (length glist) $ drop (length glist + r) $ cycle glist

main = print (rotate [1, 2, 3, 4] 3)