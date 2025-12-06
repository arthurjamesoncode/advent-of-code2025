module Part2 (part2) where

import Data.List
-- import Debug.Trace
import Parse (parse)

-- part2 :: String -> Int
part2 input = 
  foldl' (\acc bank -> acc + (read $ get_max_voltage bank)) 0 parsed
  where parsed = parse input

max_before :: Int -> String -> (Char, Int)
max_before = max_before_helper 0 ('0', 0) 

max_before_helper :: Int -> (Char, Int) -> Int -> String -> (Char, Int)
max_before_helper _        curr_max _   []     = curr_max
max_before_helper curr_pos curr_max end (x:xs)
  | curr_pos > end  = curr_max
  | x > fst curr_max = max_before_helper (curr_pos + 1) (x, curr_pos) end xs
  | otherwise        = max_before_helper (curr_pos + 1) curr_max end xs

get_max_voltage :: String -> String
get_max_voltage = get_max_voltage_helper 12

get_max_voltage_helper :: Int -> String -> String
get_max_voltage_helper 0         _    = ""
get_max_voltage_helper _         ""   = ""
get_max_voltage_helper remaining bank =
  max_char:(get_max_voltage_helper (remaining - 1) . drop (max_pos + 1) $ bank)
  where
    l                   = length bank
    (max_char, max_pos) = max_before (l - remaining) bank
    -- message             = "Remaining: " ++ show remaining ++ " Bank: " ++ bank ++ " Max: " ++ show max_char
