import Data.Text
import Data.Maybe
import qualified Data.Map.Strict as Map

upto20 = ["", "one ", "two ", "three ", "four ", "five ", "six ", "seven ", "eight ", "nine ", "ten ", "eleven ", "twelve ", "thirteen ", "fourteen ", "fiften ", "sixteen ", "seventeen ", "eighteen ", "nineteen "]
tens = ["", "", "twenty ", "thirty ", "fourty ", "fifty ", "sixty ", "seventy ", "eighty ", "ninety "]

hun = "hundred "
han = "hundred and "

westernDenoms = [10^12, 10^9, 10^6, 10^3, 1]
w_denomNames = ["Trillion ", "Billion ", "Million ", "Thousand ", ""]
ws = Map.fromList $ Prelude.zip westernDenoms w_denomNames

indianDenoms = [10^7, 10^5, 10^3, 1]
i_denomNames = ["Crores ", "Lakhs ", "Thousands ", ""]
is = Map.fromList $ Prelude.zip indianDenoms i_denomNames  

convert2digits :: Int -> String
convert2digits n | n < 20    = upto20 !! n
convert2digits n | otherwise = (tens !! (div n 10)) ++ (upto20 !! (mod n 10))

convert3digits :: Int -> String
convert3digits n | n < 100        = convert2digits n
convert3digits n | mod n 100 == 0 = convert2digits (div n 100) ++ hun
convert3digits n | otherwise      = convert2digits h ++ han ++ convert2digits tu where
    h = div n 100
    tu = mod n 100

splitIntoDenoms :: Int -> [Int] -> [(Int, Int)]
splitIntoDenoms amount denoms | Prelude.length denoms == 1 = [(amount, Prelude.head denoms)]
splitIntoDenoms amount denoms | otherwise          = (quot amount d, d) : splitIntoDenoms (mod amount d) (Prelude.tail denoms) where d = Prelude.head denoms

convertToWestern :: (Int, Int) -> String
convertToWestern (0, b) = ""
convertToWestern (a, b) = convert3digits a ++ (Data.Maybe.fromMaybe "" ( Map.lookup b ws))

convertToIndian :: (Int, Int) -> String
convertToIndian (0, denoms) = ""   
convertToIndian (amount, denoms) = convert3digits amount ++ (Data.Maybe.fromMaybe "" (Map.lookup denoms is))

fig2Words :: Int -> [Int] -> String
fig2Words amount denoms | denoms == westernDenoms = Prelude.concat (Prelude.map convertToWestern (splitIntoDenoms amount denoms))
fig2Words amount denoms | denoms == indianDenoms  = Prelude.concat (Prelude.map convertToIndian (splitIntoDenoms amount denoms))


main = do
   -- n <- getLine
    --print $ Main.split (read n :: Int)
    print $ fig2Words 123456789 westernDenoms 
    print $ fig2Words 123456789 indianDenoms

-- p10 = map (10^) [0, 1..]