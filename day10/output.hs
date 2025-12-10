import Part1 (part1)
import Part2 (part2)

main = do
  test  <- readFile "test.txt"
  input <- readFile "input.txt"

  putStrLn $ "Part 1:"
  putStrLn $ "Test  " ++ (show . part1 $ test)
  -- putStrLn $ "Input " ++ (show . part1 $ input) ++ "\n"

  putStrLn $ "Part 2:"
  putStrLn $ "Test  " ++ (show . part2 $ test)
  -- putStrLn $ "Input " ++ (show . part2 $ input) ++ "\n"
