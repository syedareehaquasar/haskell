import Data.Char (toUpper)

generateHashtag :: String -> Maybe String
generateHashtag "" = Nothing
generateHashtag s | (sum $ map (length) s) > 140 = False
                  | "#" ++ toUpper(head s1)