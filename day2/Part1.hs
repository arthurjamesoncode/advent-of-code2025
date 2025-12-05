module Part1 (part1) where

import Data.List 
import Parse (parse)

-- part1 :: String -> Int
part1 input = 
  foldl' (\acc range -> 
    let
      (r_min, r_max) = range
      first          = get_first_invalid r_min
      total          = sum_invalids first r_max
      should_check   = invalids_can_exist range
    in if should_check then acc + total else acc
    ) 0 parsed
  where parsed = parse input

is_invalid :: String -> Bool
is_invalid str = 
  even l && front == back
  where
    l     = length str
    half  = l `div` 2 
    front = take half str
    back  = drop half str

next_invalid :: String -> String
next_invalid str = 
  show (front + 1) ++ show (back + 1)
  where
    half  = (length str) `div` 2 
    front = read . take half $ str :: Int
    back  = read . drop half $ str :: Int

invalids_can_exist :: (Int, Int) -> Bool
invalids_can_exist (r_min, r_max) =
  even l_min || even l_max || l_min /= l_max
  where 
    l_min = length . show $ r_min
    l_max = length . show $ r_max

get_first_invalid :: Int -> String
get_first_invalid r_min
  | is_invalid str = str
  | back < front   = show front ++ show front
  | otherwise      = show (front + 1) ++ show (front + 1)
  where
    str   = show r_min
    l     = length str
    start = if even l then str else '1':replicate l '0'
    half  = (length start) `div` 2
    front = read . take half $ start :: Int
    back  = read . drop half $ start :: Int

sum_invalids :: String -> Int -> Int
sum_invalids invalid r_max 
  | num > r_max = 0
  | otherwise   = num + (sum_invalids next r_max)
  where 
    num  = read invalid :: Int
    next = next_invalid invalid