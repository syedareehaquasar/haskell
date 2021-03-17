discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = []
discard p (x:xs) = if not (p x) then x : discard p xs else discard p xs

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p (x:xs) = if p x then x : keep p xs else keep p xs
