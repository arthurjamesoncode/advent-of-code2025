module Part2 (part2) where

import Data.List
import Parse (parse, Point)
import Part1 (get_pairs, sort_pairs, add_pair_to_circuits)

-- part2 :: String -> Int
part2 input = (floor x1) * (floor x2)
  where
    points                   = parse input
    num_points               = length points 
    sorted_pairs             = sortBy sort_pairs . get_pairs $ points
    ((x1, _, _), (x2, _, _)) = add_until_full sorted_pairs [] num_points 

add_until_full :: [(Point, Point)] -> [[Point]] -> Int -> (Point, Point)
add_until_full (x:xs) circuits num_points
  | only_one && contains_all = x
  | otherwise                = add_until_full xs new_circuits num_points
  where 
    new_circuits   = add_pair_to_circuits circuits x
    only_one       = null . tail $ new_circuits
    circuit        = head new_circuits
    contains_all   = length circuit == num_points