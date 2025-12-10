module Part2 (part2) where

import Data.List
import Part1 (get_pairs)
import Parse (parse2, Instruction2)


--I gave up on this one. I downloaded someones solution. 
--I need to learn about ILP apparently
part2 :: String -> Int
part2 input =
  map get_fewest_presses instructions
  where
    instructions = map sort_buttons . parse2 $ input

sort_buttons :: Instruction2 -> Instruction2
sort_buttons (buttons, joltage) = 
  (new_buttons, joltage)
  where
    new_buttons = sortBy (\a b -> compare (get_button_val b) (get_button_val a)) buttons

get_button_val :: [Int] -> Int
get_button_val = sum . map (2^)

update_joltage :: [Int] -> [Int] -> [Int]
update_joltage = update_joltage_helper 0

update_joltage_helper :: Int -> [Int] -> [Int] -> [Int]
update_joltage_helper _ _      []    = []
update_joltage_helper i button (x:xs) 
  | i `elem` button = (x-1):update_joltage_helper (i+1) button xs
  | otherwise       = x:update_joltage_helper (i+1) button xs

get_max_pressed :: Instruction2 -> ([([Int], Int)], [Int])
get_max_pressed (buttons, joltage) =
  (new_buttons, joltage)
  where
    new_buttons = map (\b -> (b, minimum . map (joltage!!) $ b)) buttons

get_fewest_presses instruction = 
  get_fewest_presses_helper buttons [joltage] 0
  where
    (buttons, joltage) = instruction

get_fewest_presses_helper buttons values presses 
  | any (all (==0)) new_values = presses + 1
  | otherwise                  = get_fewest_presses_helper buttons new_values $! (presses + 1)
  where 
    new_values = filter (all (>= 0)) . map (uncurry update_joltage) . get_pairs buttons $ values
