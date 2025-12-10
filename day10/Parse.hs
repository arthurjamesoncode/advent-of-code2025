module Parse (parse, Instruction) where

type Instruction = (Int, [Int], String)

parse :: String -> [Instruction]
parse input = map get_instruction . lines $ input
    
get_instruction :: String -> Instruction
get_instruction str = (target, buttons, joltage_str)
  where
    w                  = words str
    diagram            = head w
    (button_strs, joltage_str) = get_rest . tail $ w
    target             = diagram_to_num diagram
    buttons            = map button_to_num button_strs
    --ignoring joltage for the time being

get_rest :: [String] -> ([String], String)
get_rest [x]    = ([], x)
get_rest (x:xs) = (x:front, back)
  where
    (front, back) = get_rest xs

diagram_to_num :: String -> Int
diagram_to_num = diagram_to_num_helper 0

diagram_to_num_helper :: Int -> String -> Int
diagram_to_num_helper digits (x:xs)
  | x == ']'  = 0
  | x == '['  = diagram_to_num_helper digits xs
  | x == '.'  = diagram_to_num_helper (digits + 1) xs      
  | otherwise = val + diagram_to_num_helper (digits + 1) xs 
  where
    l   = length xs
    val = 2^digits

button_to_num :: String -> Int
button_to_num (x:xs)
  | x == ')'             = 0
  | x == '(' || x == ',' = button_to_num xs
  | otherwise            = val + button_to_num xs
  where
    val = 2^(read [x])