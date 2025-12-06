module Part2 (part2) where

import Data.Array
import Data.List
import Debug.Trace
import Parse (parse)
import Part1 (get_accessible)

part2 :: String -> Int
part2 input = remove_rolls grid
  where grid = parse input

remove_rolls :: Array Int (Array Int Char) -> Int
remove_rolls grid
  | removed == 0 = 0
  | otherwise    = removed + (remove_rolls new_grid)
  where
    accessible = get_accessible grid 0
    removed    = length accessible
    positions  = format_positions accessible
    new_grid   = reduce_grid grid positions

format_positions :: [(Int, Int)] -> [(Int, [Int])]
format_positions positions =
  map (\row -> 
    let 
      row_index = fst . head $ row
    in (row_index, map snd row)
    ) groups
  where
    groups = groupBy (\(row1, _) (row2, _) -> row1 == row2) positions

reduce_grid :: Array Int (Array Int Char) -> [(Int, [Int])] -> Array Int (Array Int Char)
reduce_grid grid positions = grid//[(row, (modify_row (grid ! row) cols)) | (row, cols) <- positions]

modify_row :: Array Int Char -> [Int] -> Array Int Char
modify_row row cols = row//[(col, '.') | col <- cols]