module Parse (parse) where

parse :: String -> [(Int, Int)]
parse input = map get_instruction . lines $ input

get_instruction :: String -> (Int, Int)
get_instruction []             = error "BAD"
get_instruction (char:num_str) = (dir, num)
  where dir = if char == 'R' then 1 else -1
        num = read num_str
