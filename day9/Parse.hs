module Parse (parse) where

parse :: String -> [(Int, Int)]
parse input = map get_point . lines $ input

get_point :: String -> (Int, Int)
get_point str = (col, row)
  where
    (front, back) = span (/=',') str
    col           = read front
    row           = read . drop 1 $ back
