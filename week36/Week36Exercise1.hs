module Week36Exercise1 where

-- checks if sum of list is less than y
f :: [Integer] -> Integer -> Bool
f x y = y > sum x
