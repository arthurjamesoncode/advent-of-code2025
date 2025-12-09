module Part1 (part1, get_pairs, get_area) where

import Data.List
import Parse (parse)

part1 :: String -> Int
part1 input = head sorted
  where
    points  = parse input
    pairs   = get_pairs points
    areas   = map (uncurry get_area) pairs
    sorted  = sortBy (\a b -> compare b a) areas

get_pairs :: [a] -> [(a, a)]
get_pairs []     = []
get_pairs (a:as) = map (\b -> (a, b)) as ++ get_pairs as

get_area :: (Int, Int) -> (Int, Int) -> Int
get_area (x1, y1) (x2, y2) = 
  width * height
  where
    width  = 1 + abs (x2 - x1) 
    height = 1 + abs (y2 - y1)