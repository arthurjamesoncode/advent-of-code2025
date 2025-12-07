module Part2 (part2) where

import Data.List
import Parse (parse)

part2 :: String -> Int
part2 input = 
  sum_ranges sorted
  where
    (ranges, _) = parse input
    sorted = sort ranges

--relies on the ranges being sorted
sum_ranges :: [(Int, Int)] -> Int
sum_ranges = sum_ranges_helper 0

sum_ranges_helper :: Int -> [(Int, Int)] -> Int
sum_ranges_helper _           []                  = 0
sum_ranges_helper max_checked ((r_min, r_max):xs) 
  | whole = (r_max - r_min + 1) + sum_ranges_helper r_max xs
  | part  = (r_max - max_checked) + sum_ranges_helper r_max xs
  | otherwise = sum_ranges_helper max_checked xs
  where
    min_below = r_min <= max_checked
    max_below = r_max <= max_checked
    whole     = not min_below
    part      = min_below && not max_below