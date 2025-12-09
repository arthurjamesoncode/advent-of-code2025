module Part2 (part2) where

import Data.List
import Part1 (get_pairs, get_area)
import Parse (parse)

type Point = (Int, Int)
type Edge = (Point, Point)
type Bounds = (Point, Point)

-- part2 :: String -> Int
-- Again I have given up with haskell. Did this one in python instead of JS.
part2 input = maximum areas
  where
    points      = parse input
    bounds      = get_bounds points
    edges       = get_edges points
    pairs       = get_pairs points
    valid_pairs = filter (area_valid bounds edges) pairs
    areas       = map (uncurry get_area) valid_pairs

get_bounds :: [Point] -> Bounds
get_bounds points = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where 
    xs = map fst points
    ys = map snd points

get_edges :: [Point] -> [Edge]
get_edges points = get_edges_helper points $ head points

get_edges_helper :: [Point] -> Point -> [Edge]
get_edges_helper [x]      first = [(x, first)]
get_edges_helper (x:y:xs) first = (x, y):get_edges_helper (y:xs) first

point_in_polygon :: Bounds -> [Edge] -> Point -> Bool
point_in_polygon bounds edges (x, y)
  | on_boundary   = True
  | out_of_bounds = False
  | otherwise     = odd intersections
  where
    ((x_min, y_min), (x_max, y_max)) = bounds
    out_of_bounds = x < x_min || x > x_max || y < y_min || y > y_max
    intersections = sum_ray_intersects (x, y) edges
    on_boundary   = is_on_boundary (x, y) edges

is_on_boundary :: Point -> [Edge] -> Bool
is_on_boundary p edges = any (point_on_segment p) edges

point_on_segment :: Point -> Edge -> Bool
point_on_segment (px, py) ((x1, y1), (x2, y2))
    | x1 == x2 = 
        px == x1 && py >= min y1 y2 && py <= max y1 y2
    | y1 == y2 = 
        py == y1 && px >= min x1 x2 && px <= max x1 x2
    | otherwise = False 

sum_ray_intersects :: Point -> [Edge] -> Int
sum_ray_intersects _ [] = 0
sum_ray_intersects origin (edge:edges)
    | is_horizontal = sum_ray_intersects origin edges
    | p_y < minY || p_y >= maxY = sum_ray_intersects origin edges
    | p_x < p1_x = 1 + sum_ray_intersects origin edges
    | otherwise = sum_ray_intersects origin edges
  where
    (p_x, p_y) = origin
    (x1, y1) = fst edge
    (x2, y2) = snd edge

    (p1_x, p1_y, p2_x, p2_y) = if y1 < y2 then (x1, y1, x2, y2) else (x2, y2, x1, y1)

    minY = p1_y
    maxY = p2_y

    is_horizontal = y1 == y2

-- area_valid :: Bounds -> [Edge] -> (Point, Point) -> Bool
area_valid bounds edges ((x1, y1), (x2, y2)) =
  a && b && intersects
  where
    a          = point_in_polygon bounds edges (x1, y2)
    b          = point_in_polygon bounds edges (x2, y1)
    rect_edges = [((x1, y1), (x1, y2)), ((x1, y1), (x2, y1)), ((x1, y2), (x2, y2)), ((x2, y1), (x2, y2))]
    intersects = any (\edge -> any (edges_intersect edge) rect_edges) edges


edges_intersect :: Edge -> Edge -> Bool
edges_intersect edge1 edge2 
  | not different = False
  | horizontal1   = y1_1 < max y2_1 y2_2 && y1_1 > min y2_1 y2_2
  | horizontal2   = y2_1 < max y1_1 y1_2 && y2_1 > min y1_1 y1_2
  where
    ((x1_1, y1_1), (x2_1, y2_1)) = edge1
    ((x1_2, y1_2), (x2_2, y2_2)) = edge2
    horizontal1 = (x1_1 == x2_1)
    horizontal2 = (x1_2 == x2_2)
    different   = horizontal1 /= horizontal2