module Parse (parse) where

parse :: String -> [(Int, Int)]
parse input = map get_range . split_by_comma $ input

split_by_comma :: String -> [String]
split_by_comma ""  = [] 
split_by_comma str = 
  front:(split_by_comma . drop 1 $ rest)
  where
    (front, rest) = span (/=',') str

get_range :: String -> (Int, Int)
get_range str = 
  (r_min, r_max)
  where
    r_min = read . takeWhile (/='-') $ str
    r_max = read . drop 1 . dropWhile (/='-') $ str
