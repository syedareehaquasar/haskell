{-
 - 12-10-2020
 - Euler problem 17
 -
 - If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 - If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
 - NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
 -}

ones = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["", "", "twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"]

numberToWords :: Int -> [Char]
numberToWords n | n < 0 = "Not possible"
numberToWords n | n < 20 = ones !! n
numberToWords n | n < 100 = (tens !! (div n 10)) ++ (if (mod n 10 /= 0) then (ones !! (mod n 10)) else "")
numberToWords n | n < 1000 = (ones !! (div n 100)) ++ "hundred" ++ (if (mod n 100 /= 0) then ("and" ++ numberToWords (mod n 100)) else "")
numberToWords n | n < 1000000 = numberToWords (div n 1000) ++ "thousand" ++ (if (mod n 1000 /= 0) then (numberToWords (div n 1000)) else "")

main = do
    print (sum [length (numberToWords n) | n <- [1..1000]])
