module Part2 (part2) where

import Data.List
import Parse (parse)

part2 :: String -> Int
part2 input = 
  snd . foldl' (\(pos, count) (dir, distance) -> 
    let
      move      = dir * distance
      new_pos   = (pos + move) `mod` 100
      new_count = count + pointed_at_0 pos move
    in (new_pos, new_count)
    ) (50, 0) $ parsed
  where parsed = parse input

pointed_at_0 :: Int -> Int -> Int
pointed_at_0 pos move 
  | pos == 0 || not add_one = rotations
  | otherwise               = rotations + 1
  where 
    rotations   = (abs move) `div` 100
    remaining   = (abs move) `mod` 100
    click_left  = remaining >= pos && move < 0
    click_right = remaining >= 100 - pos && move > 0
    add_one = click_left || click_right
