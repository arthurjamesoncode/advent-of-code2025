x=1
while [ -d "day$x" ]
do
  x=$(( $x + 1 ))
done

mkdir "day$x"

read -r -d '' PARSE_CONTENT << END
module Parse (parse) where

parse :: String -> String
parse input = input 
END
echo "$PARSE_CONTENT" > "day$x/Parse.hs"

read -r -d '' PART1_CONTENT << END
module Part1 (part1) where

import Parse (parse)

part1 :: String -> Int
part1 input = 0 
END
echo "$PART1_CONTENT" > "day$x/Part1.hs"

read -r -d '' PART2_CONTENT << END
module Part2 (part2) where

import Parse (parse)

part2 :: String -> Int
part2 input = 0 
END
echo "$PART2_CONTENT" > "day$x/Part2.hs"

read -r -d '' OUTPUT_CONTENT << END
import Part1 (part1)
import Part2 (part2)

main = do
  test  <- readFile "test.txt"
  input <- readFile "input.txt"

  putStrLn $ "Part 1:"
  putStrLn $ "Test  " ++ (show . part1 $ test)
  putStrLn $ "Input " ++ (show . part1 $ input) ++ "\n"

  putStrLn $ "Part 2:"
  putStrLn $ "Test  " ++ (show . part2 $ test)
  putStrLn $ "Input " ++ (show . part2 $ input) ++ "\n"
END
echo "$OUTPUT_CONTENT" > "day$x/output.hs"

touch "day$x/input.txt"
touch "day$x/test.txt"

read -r -d '' NEXT_DAY_CONTENT << END
cd ..
bash init_day.sh
END
echo "$NEXT_DAY_CONTENT" > "day$x/next_day.sh"

code "day$x"