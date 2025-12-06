module Parse (parse) where

import Data.Array

parse :: String -> Array Int (Array Int Char)
parse input = listArray (0, height) $ map (listArray (0, width)) ls
  where 
    ls     = lines input
    height = length ls - 1
    width  = (length . head $ ls) - 1
