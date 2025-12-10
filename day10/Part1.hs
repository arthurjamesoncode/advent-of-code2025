module Part1 (part1) where

import Debug.Trace
import Data.Bits
import Parse (parse, Instruction)

-- part1 :: String -> Int
part1 input =sum . map get_fewest_presses $ instructions
  where
    instructions = parse input

get_fewest_presses :: Instruction -> Int
get_fewest_presses instruction = get_fewest_presses_helper instruction [0] 0 

get_fewest_presses_helper instruction values presses 
  | any (==target) possible = presses + 1
  | otherwise               = get_fewest_presses_helper instruction possible $ (presses + 1)
  where
    (target, buttons, _) = instruction
    possible             = get_new_values buttons values 

get_new_values :: [Int] -> [Int] -> [Int]
get_new_values buttons values =
  map (uncurry xor) combos
  where
    combos = get_pairs buttons values

get_pairs :: [a] -> [b] -> [(a, b)]
get_pairs []     _   = []
get_pairs (x:xs) ys = map (\y -> (x, y)) ys ++ get_pairs xs ys



