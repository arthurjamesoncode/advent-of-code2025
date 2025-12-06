module Parse (parse) where

parse :: String -> ([(Int, Int)], [Int])
parse input = (ranges, ids)
  where
    (str_ranges, str_ids) = splitOn "" . lines $ input
    ranges                = map (\s ->  
                                  let
                                    (r_min, r_max) = (splitOn '-' s) 
                                  in (read r_min, read r_max)
                                ) str_ranges
    ids                   = map read str_ids

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn e xs = 
  (front, drop 1 back)
  where
    (front, back) = span (/=e) xs