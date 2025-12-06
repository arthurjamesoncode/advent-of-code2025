module Part1 (part1, get_accessible) where

import Data.Array
import Debug.Trace
import Parse (parse)

part1 :: String -> Int
part1 input = length . get_accessible parsed $ 0
  where 
    parsed = parse input

get_accessible :: Array Int (Array Int Char) -> Int -> [(Int, Int)]
get_accessible grid index
  | index >= (width * height)   = []
  | char == '@' && accessible = (row, col):rest
  | otherwise                 = rest
  where
    (_, max_row) = bounds grid
    (_, max_col) = bounds (grid ! 0)
    height       = max_row + 1
    width        = max_col + 1    
    row          = (index `div` height)
    col          = (index `mod` width)
    char         = grid ! row ! col
    surrounding  = count_surrounding grid row col
    accessible   = surrounding < 4
    rest         = get_accessible grid (index + 1)

directions = [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]

count_surrounding :: Array Int (Array Int Char) -> Int -> Int -> Int
count_surrounding grid row col =
  length . filter (=='@') $ get_adjacent grid row col

get_adjacent :: Array Int (Array Int Char) -> Int -> Int -> [Char]
get_adjacent grid row col = 
  map (\(dx, dy) -> 
    let
      new_row   = row + dy
      new_col   = col + dx
      valid_row = new_row <= height && new_row >= 0
      valid_col = new_col <= width && new_col >= 0
    in 
      if valid_col && valid_row 
        then grid ! new_row ! new_col
        else '!'
    ) $ directions
  where
    (_, height) = bounds grid
    (_,  width) = bounds (grid ! 0)