import Data.Char (toUpper, isUpper)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

vowels = "aiyeou" ++ "aiyeou" ++ "AIYEOU" ++ "AIYEOU"
consonants = "bkxznhdcwgpvjqtsrlmf" ++ "bkxznhdcwgpvjqtsrlmf" ++ "BKXZNHDCWGPVJQTSRLMF" ++ "BKXZNHDCWGPVJQTSRLMF"

encode :: Char -> Char
encode c | (elem c consonants) = consonants !! (fromJust(elemIndex c consonants) + 10)
         | (elem c vowels) = vowels !! (fromJust(elemIndex c vowels) + 3)
         | otherwise = c

tongues :: String -> String
tongues str = map encode str

main = do
    print $ tongues "One ring to rule them all."