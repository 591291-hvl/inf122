
primes :: Integer -> [Integer]
primes n = filter (/=0) [if (length $ filter (==0) [x `mod` y| y <- [1..n]]) <= 2 then x else 0 | x <- [1..n]]

main = print(primes 100)