module Part1 (part1, get_pairs, sort_pairs, add_pair_to_circuits) where

import Data.List
import Parse (parse, Point)

part1 :: String -> Int
part1 input = product . take 3 . sortBy (\a b -> compare b a) . map length $ circuits
  where
    points          = parse input
    num_pairs       = if length points > 20 then 1000 else 10
    sorted_pairs    = sortBy sort_pairs . get_pairs $ points
    shortest        = take num_pairs sorted_pairs
    circuits        = foldl' add_pair_to_circuits [] shortest

add_pair_to_circuits :: [[Point]] -> (Point, Point) -> [[Point]]
add_pair_to_circuits circuits (point1, point2) =
  let
    point1_circuits = filter (point1 `elem`) circuits
    point2_circuits = filter (point2 `elem`) circuits

    point1_exists   = not . null $ point1_circuits
    point2_exists   = not . null $ point2_circuits

    point1_circuit  = if point1_exists then head point1_circuits else [point1]
    point2_circuit  = if point2_exists then head point2_circuits else [point2]

    both_exist      = point1_circuit == point2_circuit
    new_circuit     = point1_circuit ++ point2_circuit
    new_circuits    = if not both_exist 
                        then new_circuit:filter (\circuit -> not (point1 `elem` circuit || point2 `elem` circuit)) circuits
                        else circuits
  in new_circuits

get_pairs :: [Point] -> [(Point, Point)]
get_pairs []     = []
get_pairs (x:xs) = map (\p -> (x, p)) xs ++ get_pairs xs

get_distance :: Point -> Point -> Float
get_distance (x1, y1, z1) (x2, y2, z2) =
  sqrt((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)

sort_pairs :: (Point, Point) -> (Point, Point) -> Ordering
sort_pairs pair1 pair2
  | distance1 < distance2 = LT
  | distance1 > distance2 = GT
  | otherwise             = EQ
  where 
    distance1 = uncurry get_distance $ pair1
    distance2 = uncurry get_distance $ pair2