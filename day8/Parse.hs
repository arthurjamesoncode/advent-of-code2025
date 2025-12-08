module Parse (parse, Point) where

type Point = (Float, Float, Float)

parse :: String -> [Point]
parse input = 
  map (\l ->
    let
      [x, y, z] = map read . split_by_comma $ l
    in (x, y, z)
      ) ls 
  where
    ls = lines input

split_by_comma ""  = []
split_by_comma str = 
  front:(split_by_comma . drop 1 $ back)
  where
    (front, back) = span (/=',') str