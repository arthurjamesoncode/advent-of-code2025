module Part2 (part2) where

import Data.List
import Parse (parse)

--Out of indignity I have commented out the code I was trying to get to work. 
--I did get it to work on the test case, but not the full input.
--Turns out a fucking brute force algorithm is sufficient to just do everything. Just takes like 10 seconds.
--At this, moment in time. I actually would like the sweet release of death. FUCK ME.
part2 :: String -> Int
part2 input =
  foldl' (\acc range ->
    let
      (r_min, r_max) = range
      -- (l_min, l_max) = (length . show $ r_min, length . show $ r_max)
      -- ranges = if l_min == l_max then [(r_min, r_max)] else [(r_min, 10^l_min - 1), (10^l_min, r_max)]
      -- add = sum . map (uncurry sum_invalids) $ ranges
      add = sum . filter (is_invalid . show) $ [r_min..r_max]
    in acc + add
    ) 0 parsed
  where parsed = parse input

is_invalid :: String -> Bool
is_invalid str =
  any (\x -> x) . map (section_repeats str) $ factors
  where
    factors    = get_factors $ length str

section_repeats :: String -> Int -> Bool
section_repeats str divider =
  repeated == str
  where
    l        = length str
    sections = l `div` divider
    first    = take divider str
    repeated = concatMap (\_ -> first) . replicate sections $ '.'

get_factors :: Int -> [Int]
get_factors num 
  | null factors          = []
  | null . tail $ factors = factors
  | otherwise             = tail factors
  where
    factors = filter (\x -> num `mod` x == 0) [1..num `div` 2]

-- get_first_invalid :: Int -> Int -> String
-- get_first_invalid r_min factor
--   | section_repeats str factor = str
--   | maximum sections == front      = concatMap (\_ -> show front) . replicate repeats $ '.'
--   | otherwise                  = concatMap (\_ -> show (front + 1)) . replicate repeats $ '.'
--   where
--     str      = show r_min
--     l        = length str
--     repeats  = l `div` factor
--     front    = read . take factor $ str :: Int
--     next     = read . take factor . drop factor $ str :: Int
--     sections = map (\x -> read x :: Int) (separate str [] factor factor)

-- separate :: [a] -> [a] -> Int -> Int -> [[a]]
-- separate []     section _      _    = [section]
-- separate ls     section factor 0    = section:separate ls [] factor factor 
-- separate (x:xs) section factor curr = separate xs (section ++ [x]) factor (curr-1) 

-- sum_invalids :: Int -> Int -> Int
-- sum_invalids r_min r_max =
--   sum . uniq . concatMap (uncurry . all_invalids $ r_max) $ firsts
--   where
--     str     = show r_min
--     factors = get_factors . length $ str
--     firsts  = map (\x -> (get_first_invalid r_min x, x)) factors

-- uniq :: Eq a => [a] -> [a]
-- uniq [] = []
-- uniq (x:xs) = x:uniq (filter (/=x) xs)

-- all_invalids :: Int -> String -> Int -> [Int]
-- all_invalids r_max first factor 
--   | not . is_invalid $ first = []
--   | r_max < num              = []
--   | otherwise                = num:all_invalids r_max next factor 
--   where 
--     num  = (read first :: Int)
--     next = next_invalid first factor

-- next_invalid :: String -> Int -> String
-- next_invalid str factor = 
--   concatMap (\_ -> show (front+1)) . replicate sections $ '.'
--   where
--     sections = (length str) `div` factor  
--     front    = read . take factor $ str :: Int



  