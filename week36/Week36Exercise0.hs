module Week36Exercise0 where

--Checks if sum of x and y is not an even number
f :: Integer -> Integer -> Bool
f x y = not $ even (x + y)
