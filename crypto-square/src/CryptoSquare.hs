module CryptoSquare (encode) where

import Data.Char(toLower, isAlphaNum)
import Data.List(intercalate, transpose)
import Data.List.Split(chunksOf)

findRC :: String -> Int
findRC s = floor . sqrt . fromIntegral . length $ s

normalise :: String -> String
normalise s = map toLower $ filter isAlphaNum s

encode :: String -> String
encode xs = intercalate " " $ transpose $ chunksOf (findRC xs) $ normalise xs
