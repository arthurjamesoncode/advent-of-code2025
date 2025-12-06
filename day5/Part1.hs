module Part1 (part1) where

import Parse (parse)

part1 :: String -> Int
part1 input =
  length . filter (is_fresh ranges) $ ids
  where
    (ranges, ids) = parse input

is_fresh :: [(Int, Int)] -> Int -> Bool
is_fresh ranges id = 
  not . null . filter (\(r_min, r_max) -> r_min <= id && r_max >= id) $ ranges


