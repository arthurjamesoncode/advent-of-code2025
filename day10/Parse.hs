module Parse (parse, Instruction, parse2, Instruction2) where

type Instruction = (Int, [Int])
type Instruction2 = ([[Int]], [Int])

parse :: String -> [Instruction]
parse input = map get_instruction . lines $ input
    
get_instruction :: String -> Instruction
get_instruction str = (target, buttons)
  where
    w                  = words str
    diagram            = head w
    button_strs = fst . get_rest . tail $ w
    target             = diagram_to_num diagram
    buttons            = map button_to_num button_strs
    

parse2 :: String -> [Instruction2]
parse2 input = map get_instruction2 . lines $ input

get_instruction2 :: String -> Instruction2
get_instruction2 str = (buttons, joltage)
  where
    w           = words str
    rest        = get_rest . tail $ w
    button_strs = fst rest
    buttons     = map (\str -> map read . split_by_comma . drop 1 . take (length str - 1) $ str) button_strs
    joltage_str = snd rest
    joltage     = map read . split_by_comma . drop 1 . take (length joltage_str - 1) $ joltage_str

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
    val = 2^digits

joltage_to_num :: [String] -> Int
joltage_to_num = joltage_to_num_helper 0

joltage_to_num_helper :: Int -> [String] -> Int
joltage_to_num_helper _      []     = 0
joltage_to_num_helper digits (x:xs) = 
  val + joltage_to_num_helper (digits + 1) xs 
  where
    val = (2^digits) * (read x :: Int)

split_by_comma "" = []
split_by_comma xs = 
  front:split_by_comma (drop 1 back)
  where
    (front, back) = span (/=',') xs

button_to_num :: String -> Int
button_to_num (x:xs)
  | x == ')'             = 0
  | x == '(' || x == ',' = button_to_num xs
  | otherwise            = val + button_to_num xs
  where
    val = 2^(read [x])