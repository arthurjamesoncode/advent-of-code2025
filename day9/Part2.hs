module Part2 (part2) where

import Data.List
import Part1 (get_area)
import Parse (parse)

type Point = (Int, Int)
type Edge = (Point, Point)
type Bounds = (Point, Point)

-- part2 :: String -> Int
-- Again I have given up with haskell. Did this one in python instead of JS.
part2 input = get_max_area pairs edges
  where
    points      = parse input
    edges       = get_edges points
    pairs       = get_sorted_pairs points

get_sorted_pairs :: [Point] -> [(Point, Point)]
get_sorted_pairs []     = []
get_sorted_pairs (a:as) = 
  map (\b -> 
    let 
      x1 = min (fst a) (fst b)
      y1 = min (snd a) (snd b)
      x2 = max (fst a) (fst b)
      y2 = max (snd a) (snd b)
    in ((x1, y1), (x2, y2))
    ) as ++ get_sorted_pairs as

get_max_area :: [(Point, Point)] -> [Edge] -> Int
get_max_area = get_max_area_helper 0

get_max_area_helper :: Int -> [(Point, Point)] -> [Edge] -> Int
get_max_area_helper curr_max []           _     = curr_max
get_max_area_helper curr_max (pair:pairs) edges =
  get_max_area_helper new_max pairs edges
  where
    area = uncurry get_area $ pair
    new_max = if area > curr_max 
                then if in_bounds then area else curr_max
                else curr_max
    in_bounds = is_in_shape edges pair

is_in_shape :: [Edge] -> (Point, Point) -> Bool
is_in_shape []           _   = True
is_in_shape (edge:edges) pair 
  | out_of_bounds = False
  | otherwise = is_in_shape edges pair
  where
    ((px1, py1), (px2, py2)) = pair
    ((x1, y1), (x2, y2))     = edge
    out_of_bounds            = x1 < px2 && y1 < py2 && x2 > px1 && y2 > py1

get_edges :: [Point] -> [Edge]
get_edges points = get_edges_helper points $ head points

get_edges_helper :: [Point] -> Point -> [Edge]
get_edges_helper [a]      b = [((x1, y1), (x2, y2))]
  where
    x1 = min (fst a) (fst b)
    y1 = min (snd a) (snd b)
    x2 = max (fst a) (fst b)
    y2 = max (snd a) (snd b)
get_edges_helper (a:b:xs) first = 
  ((x1, y1), (x2, y2)):get_edges_helper (b:xs) first
  where
    x1 = min (fst a) (fst b)
    y1 = min (snd a) (snd b)
    x2 = max (fst a) (fst b)
    y2 = max (snd a) (snd b)