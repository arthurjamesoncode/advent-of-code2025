module Part2 (part2) where

import Data.Array
import Parse (parse, Grid)
import Part1 (beam_on_splitter, get_start)

--This code doesn't work. I was trying to find some kind of way of caching results and gave up
--I can do this is imperative languages and did do it in JS for this problem. 
--I will maybe come back later and do this properly. In haskell but for today I admit defeat.

-- type ResGrid = Array (Int, Int) Int
-- emptyResGrid width height = array ((1,1), (width, height)) (take (width * height) [-1,-1..])

part2 input = 0
-- part2 :: String -> Int
-- part2 input = (snd . count_timelines empty grid row $ col) + 1
--   where
--     grid        = parse input
--     results_map = empty
--     (row, col)  = get_start grid
--     (_, height) = bounds grid
--     (_,  width) = bounds grid
--     empty       = emptyResGrid 

-- count_timelines :: ResGrid -> Grid -> Int -> Int -> (ResGrid, Int) 
-- count_timelines results grid row col
--   | result_found = (results, results ! (row, col))
--   | row > height = (results, 0)
--   | otherwise    = (newMap, total)
--   where
--     key = (row, col)
--     result_found           = results ! (row, col) /= (-1)
--     (_, height)            = bounds grid
--     splitting              = beam_on_splitter grid row col
--     (leftMap, leftTotal)   = count_timelines results grid row (col - 1)
--     (rightMap, rightTotal) = count_timelines results grid row (col + 1)
--     (backResults, total)   = if splitting 
--                               then (leftMap `union` rightMap, 1 + leftTotal + rightTotal)
--                               else count_timelines results grid (row + 1) col
--     newResults             = insert (row, col) total backMap

-- merge_result_grids