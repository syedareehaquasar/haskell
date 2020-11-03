--rotating a list

rotate :: [a] -> Int -> [a]
rotate glist r = take n $ drop (n + r) $ cycle glist where n = length glist

main = print (rotate [1, 2, 3, 4] 3)