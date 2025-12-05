module Part1 (part1) where

import Data.List
import Parse (parse)

-- part1 :: String -> Int
part1 input = 
  snd . foldl' (\(pos, count) (dir, distance) -> 
    let
      move      = dir * distance
      new_pos   = (pos + move) `mod` 100
      new_count = if new_pos == 0 then count + 1 else count
    in (new_pos, new_count)
    ) (50, 0) $ parsed
  where parsed = parse input
