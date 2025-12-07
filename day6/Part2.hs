module Part2 (part2) where

import Parse (parse2, get_column, remove_column)

part2 :: String -> Int
part2 input = sum_totals parsed
  where
    parsed = parse2 input

sum_totals :: [[String]] -> Int
sum_totals homework
  | column == [] = 0
  | otherwise    = total + sum_totals next
  where
    column = get_column homework
    next   = remove_column homework
    total  = reduce_column column

get_nums_in_column :: [String] -> [Int]
get_nums_in_column col 
  | null . head $ col = []
  | otherwise         = num:get_nums_in_column next
  where 
    num = read . get_column $ col
    next = remove_column col

reduce_column :: [String] -> Int
reduce_column col
  | '*' `elem` operation = product nums
  | '+' `elem` operation = sum nums
  where
    operation = last col
    nums = get_nums_in_column . init $ col