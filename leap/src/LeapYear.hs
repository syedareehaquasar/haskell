module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year | rem year 4 /= 0 = False
                | rem year 400 == 0 = True
                | rem year 100 == 0 = False
                | otherwise = True

isLeapYear1 :: Integer -> Bool
isLeapYear1 year | mod year 400 == 0 = True
                 | mod year 100 == 0 = False
                 | mod year 4 == 0 = True
                 | otherwise = False
