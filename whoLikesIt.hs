likes likers | likers == [] = "no one likes this"
             | length(likers) == 1 = likers !! 0 ++ " likes this"
             | length(likers) == 2 = likers !! 0 ++ " and " ++ likers !! 1 ++ " like this"
             | length(likers) == 3 = likers !! 0 ++ ", " ++ likers !! 1 ++ " and " ++ likers !! 2 ++ " like this"
             | otherwise = likers !! 0 ++ ", " ++ likers !! 1 ++ " and " ++ show(length(likers) - 2) ++ " others like this"

-- likes1 likers | likers == [] = "no one likes this"
--              | length(likers) == 1 = likers !! 0 ++ " likes this"
--              | length(likers) == 2 = likers !! 0 ++ " and " ++ likers !! 1 ++ " like this"
--              | length(likers) == 3 = likers !! 0 ++ ", " ++ likers !! 1 ++ " and " ++ likers !! 2 ++ " like this"
--              | otherwise = likers !! 0 ++ ", " ++ likers !! 1 ++ " and " ++ show(length(likers) - 2) ++ " others like this"

main = do
    print $ likes [] -- must be "no one likes this"
    print $ likes ["Peter"] -- must be "Peter likes this"
    print $ likes ["Jacob", "Alex"] -- must be "Jacob and Alex like this"
    print $ likes ["Max", "John", "Mark"] -- must be "Max, John and Mark like this"
    print $ likes ["Alex", "Jacob", "Mark", "Max"] -- must be "Alex, Jacob and 2 others like this"