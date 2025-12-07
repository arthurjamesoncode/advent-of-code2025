module Part1 (part1) where

import Parse (parse)

part1 :: String -> Int
part1 input = sum_totals parsed
  where 
    parsed = parse input

sum_totals :: [[String]] -> Int
sum_totals homework
  | column == [] = 0
  | otherwise    = total + sum_totals next
  where
    column = get_column homework
    next   = remove_column homework
    total  = reduce_column column

get_column :: [[String]] -> [String]
get_column []     = []
get_column (x:xs) 
  | null x    = []
  | otherwise = head x:get_column xs

remove_column :: [[String]] -> [[String]]
remove_column []     = []
remove_column (x:xs) 
  | null x    = []
  | otherwise = tail x:remove_column xs

reduce_column :: [String] -> Int
reduce_column column 
  | operation == "+" = sum nums
  | operation == "*" = product nums
  where
    operation = last column
    nums      = map read . init $ column