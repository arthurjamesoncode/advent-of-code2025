module Parse (parse) where

parse :: String -> [[String]]
parse input = map words . lines $ input
