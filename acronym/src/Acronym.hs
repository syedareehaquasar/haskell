module Acronym (abbreviate) where

import Data.Char(toUpper, isUpper)

abbreviate :: String -> String
abbreviate xs = concatMap selectLetters $ words $ map clean xs
  where 
    clean c | elem c ['-','_'] = ' '
            | otherwise = c
    selectLetters (h:t) | all isUpper t = [h]
                        | otherwise =  toUpper h : filter isUpper t
    selectLetters [] = []
