module Part1 (part1) where

import Data.List
import Parse (parse)

-- part1 :: String -> Int
part1 input = 
  foldl' (\acc bank ->
    let
      l                        = length bank
      (first_max, first_pos)   = max_and_pos bank
      last_battery             = first_pos == l
      after_first              = drop first_pos bank
      before_first             = take (first_pos - 1) bank
      (second_max, second_pos) = if last_battery 
                                  then max_and_pos before_first 
                                  else max_and_pos after_first
      max_voltage             = if last_battery 
                                  then second_max:[first_max]
                                  else first_max:[second_max]
    in read max_voltage + acc 
    ) 0 parsed
  where parsed = parse input

max_and_pos :: String -> (Char, Int)
max_and_pos = max_and_pos_helper 1 ('0', 0)

max_and_pos_helper :: Int -> (Char, Int) -> String -> (Char, Int)
max_and_pos_helper _   curr_max []     = curr_max
max_and_pos_helper pos curr_max (x:xs) 
  | x > fst curr_max = max_and_pos_helper (pos+1) (x, pos) xs
  | otherwise        = max_and_pos_helper (pos+1) curr_max xs
