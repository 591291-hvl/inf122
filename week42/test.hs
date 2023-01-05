summ :: Integer -> Integer -> Integer
summ = (+)

-- main = print $ summ 1 2
f = (+2)
xs = [1,2,3,4,5]
p = even
a = [f x | x <- xs, p x]

something ::(Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
--something pred func list =  filter pred (map func list)
--something pred func list =  (filter pred) ((map func) list)
--something pred func list  =  (filter pred) . map func $ list
--something pred func =  (filter pred) . map func
--something pred func =  (.) (filter pred) (map func)
--something pred func =  (.) (filter pred) . map $ func
--something pred =  (.) (filter pred) . map 
--something pred = (.) ((.) (filter pred)) map
--something pred = (. map) ((.) (filter pred)) 
--something pred = (. map) ((.) . (filter) $ pred)
--something pred = (. map) . (.) . (filter) $ pred
something = (. map) . (.) . (filter) 



-- something func pred list = [func x | x <- list, pred x]

-- main = print $ something even (+2) [1,2,3,4,5]


facc :: Int -> Int
facc numb = foldr (*) 1 [1..numb]

head' :: [a] -> Maybe a
head' = foldr (\a b -> Just a) Nothing


-- main = print $ facc 5

-- main = print $ head' [1,2,3,4,5]


-- sumIsEven :: Integer -> Integer -> Bool
-- sumIsEven a b = even (a + b)
-- sumIsEven a b = even . (a + ) $ b
-- sumIsEven a = even . (a +)
-- sumIsEven a = (even .) (+ a)
-- sumIsEven a = (even .) .(+) $ a
-- sumIsEven = (even .) . (+)

-- main = print $ sumIsEven 1 2

isPrime :: Integral a => a -> Bool
isPrime n = filter (\x -> (mod n x) == 0) [1..n] == [1,n]


isPrime' :: Integral a => a -> Bool
isPrime' 1 = True
isPrime' n = 1 == length (filter (\x -> (mod n x) == 0) (map snd $ filter fst $ zip (map isPrime' [1..(n-1)]) [1..(n-1)]))


-- First argument recusive call, second argument list to n
map2 :: [Bool] -> [a] -> [a]
map2 = ((map snd . filter fst) .) . zip

-- main = print $ map isPrime' [1..15]

main = print $ isPrime' 11

-- main = print $ (\n -> (map2 (map isPrime [1..n]) [1..n])) 7

-- main = print $ (\n -> filter (\x -> (mod n x) == 0) (map2 (map isPrime [1..n]) [1..(n-1)])) 9

-- main = print $ (\n -> length (filter (\x -> (mod n x) == 0) (map2 (map isPrime' [1..n]) [1..(n-1)]))) 6

