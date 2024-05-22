module Rotate (rotate) where

rotate :: Int -> [a] -> [a]
rotate n xs = take len . drop (n `mod` len) . cycle $ xs
  where len = length xs

-- doesn't work on infinite lists at all. The rotation is indefinitive in that case