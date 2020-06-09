import Data.Text
import qualified Data.Map.Strict as Map

upto20 = ["", "one ", "two ", "three ", "four ", "five ", "six ", "seven ", "eight ", "nine ", "ten ", "eleven ", "twelve ", "thirteen ", "fourteen ", "fiften ", "sixteen ", "seventeen ", "eighteen ", "nineteen "]
tens = ["", "ten ", "twenty ", "thirty ", "forty ", "fifty ", "sixty ", "seventy ", "eighty ", "ninety "]

hun = "hundred "
han = "hundred and "
-- hunT = pack "hundred "
-- hanT = pack "hundred and "

dValues = [1, 1000, 10^6, 10^9, 10^12]
dNames = ["","Thousand ", "Million ", "Billion ", "Trillion "]
denomNames = Map.fromAscList $ zip dValues dNames

-- num2dig :: Int -> [Int]
-- num2dig = map digitToInt . show

convert2digits :: Int -> String
-- convert2digits n = if n < length (upto20) then upto20[n] else tens[quot n 100] ++ "Hundred" ++ convert2digits(mod n 100)
convert2digits n | n < 20 = upto20 !! n
convert2digits n | otherwise = (tens !! (div n 10)) ++ (upto20 !! (mod n 10))

convert3digits :: Int -> String
convert3digits n | n < 100        = convert2digits n
convert3digits n | mod n 100 == 0 = convert2digits (div n 100) ++ hun
convert3digits n | otherwise      = convert2digits h ++ han ++ convert2digits tu where h = div n 100
                                                                                       tu = mod n 100

-- fix' :: Text -> Text
-- fix' s | isSuffixOf hunT s = s
-- fix' s | isInfixOf hunT s  = replace hunT hanT s                                                                                  
-- fix' s | otherwise         = s


-- fix :: String -> String
-- fix = unpack . strip . fix' . pack

split :: Int -> [Int] -> [(Int, Int)]
split amount denoms | length denoms == 1 = [(amount, head denom)]
split amount denoms | otherwise          = (d, div amount d) : split (mod amount d) (tail denoms) where d = head denoms


convert :: (Int, Int) -> String
convert (0, b) = ""
convert (a, b) = convert3digits a ++ fromJust $ Map.lookup b denomNames

fig2Words :: Int -> [Int] -> String
fig2Words amount denoms = concat map convert . split amount denoms


main = do
    n <- getLine
    print $ convert3digits (read n :: Int)



