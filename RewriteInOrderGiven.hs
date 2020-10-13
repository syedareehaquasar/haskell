module Codewars.Kata.YourOrderPlease where

import Data.Char
import Data.List

extract = filter isDigit

yourOrderPlease :: String -> String
yourOrderPlease inp = unwords $ sortOn extract $ words inp
