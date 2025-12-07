module Parse (parse, parse2, get_column, remove_column) where

parse :: String -> [[String]]
parse = map words . lines

parse2 :: String -> [[String]]
parse2 input = split_columns_by all_spaces rows
  where
    rows = lines input

get_column :: [[a]] -> [a]
get_column []     = []
get_column (x:xs) 
  | null x    = []
  | otherwise = head x:get_column xs

remove_column :: [[a]] -> [[a]]
remove_column []     = []
remove_column (x:xs) 
  | null x    = []
  | otherwise = tail x:remove_column xs

cons_column :: [a] -> [[a]] -> [[a]]
cons_column [] _          = []
cons_column (x:xs) []     = [x]:cons_column xs []
cons_column (x:xs) (y:ys) = (x:y):cons_column xs ys

span_columns :: ([[a]] -> Bool) -> [[a]] -> ([[a]], [[a]])
span_columns f rows
  | null rows = ([], [])
  | f rows    = ([], rows)
  | otherwise = (cons_column column front, back)
  where 
    (front, back) = span_columns f (remove_column rows)
    column = get_column rows

all_spaces :: [String] -> Bool
all_spaces []    = True
all_spaces (x:xs)
  | null x    = False
  | otherwise = start == ' ' && all_spaces xs
  where 
    start = head x

split_columns_by :: ([[a]] -> Bool) -> [[a]] -> [[[a]]]
split_columns_by f rows = 
  column `cons_column` (split_columns_by f . remove_column $ rest)
  where
    (column, rest) = span_columns f rows






--I need something that whenever all_spaces is true 