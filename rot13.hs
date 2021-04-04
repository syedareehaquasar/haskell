module Rot13 where

import Data.List (elemIndex, elem)
import Data.Maybe (fromJust)

helper = ['a'..'z'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['A'..'Z']

rotate :: Char -> Char
rotate x |elem x helper = helper !! (fromJust (elemIndex x helper) + 13)
         |otherwise = x

rot13 :: String -> String
rot13 "" = ""
rot13 s = map rotate s
