import Data.List(sortOn)

findMostSimilar :: [String] -> String -> String
findMostSimilar dictionary term = sortOn (length) $ zip dictionary $ map (// term) dictionary