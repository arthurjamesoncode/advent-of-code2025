module Parse (parse, Grid) where

import Data.Array

type Grid = Array Int (Array Int Char)

parse :: String -> Grid
parse input = listArray (1, height) $ map (listArray (1, width)) rows
  where 
    rows   = lines input
    height = length rows
    width  = length . head $ rows
    
