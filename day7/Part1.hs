module Part1 (part1, beam_on_splitter, get_start) where

import Data.Array
import Parse (parse, Grid)

part1 :: String -> Int
part1 input = count_nodes grid row [col]
  where
    grid  = parse input
    (row, col) = get_start grid

get_start :: Grid -> (Int, Int)
get_start = get_start_helper 1

get_start_helper :: Int -> Grid -> (Int, Int)
get_start_helper col grid 
  | char == 'S' = (1, col)
  | otherwise   = get_start_helper (col + 1) grid
  where char = grid ! 1 ! col

count_nodes :: Grid -> Int -> [Int] -> Int
count_nodes grid row beams
  | row > height  = 0
  | splitting     = length split_beams + count_nodes grid row new_beams 
  | otherwise     = count_nodes grid (row+1) beams 
  where
    (_, height)   = bounds grid
    split_beams   = filter (beam_on_splitter grid row) $ beams
    unsplit_beams = filter (not . beam_on_splitter grid row) beams
    splitting     = not . null $ split_beams
    new_beams     = uniq $ (concatMap (\col -> [col+1,col-1]) $ split_beams) ++ unsplit_beams              

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x:(uniq . filter (/=x) $ xs) 

beam_on_splitter :: Grid -> Int -> Int -> Bool
beam_on_splitter grid row col = grid ! row ! col == '^'


