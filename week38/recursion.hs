sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

powerOf :: Int -> Int -> Int
powerOf n 1 = n
powerOf n x = n * (powerOf n (x-1))

euclid :: Int -> Int -> Int
euclid x y 
    | x == y = x
    | x > y = euclid (x-y) y
    | x < y = euclid x (y-x)


lengthh :: [a] -> Int
lengthh [] = 0
lengthh [a] = 1
lengthh (a:as) = 1 + lengthh as

dropp :: Int -> [a] -> [a]
dropp n [] = []
dropp 0 a = a
dropp n (a:as) = dropp (n-1) (as)

initt :: [a] -> [a]
initt [] = []
initt [a] = []
initt (a:as) = [a] ++ initt as

andd :: [Bool] -> Bool 
andd (False:as) = False
andd [True] = True
andd (a:as) = andd as

concatt :: [[a]] -> [a]
concatt [a] = a
concatt (a:as) = a ++ concatt as

replicatee :: Int -> a -> [a]
replicatee 0 x = []
replicatee n x = [x] ++ replicatee (n-1) x

(!!!) :: [a] -> Int -> a
(!!!) (a:as) 0 = a
(!!!) (a:as) n = (!!!) as (n-1)

elemm :: Eq a => a -> [a] -> Bool
elemm x [] = False
elemm x (y:ys) 
    | x == y = True
    | x /= y = elemm x ys



countDown :: Integer -> [Integer]
countDown 0 = [0]
countDown x 
    | x < 0 = [-1]
    |otherwise = x:(countDown(x-1))


ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n+1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

--main = print(ackermann 4 0)


mergee :: Ord a => [a] -> [a] -> [a]
mergee x [] = x
mergee [] y = y
mergee (x:xs) (y:ys) 
    | x >= y = [y] ++ mergee ([x] ++ xs) (ys)
    | x < y = [x] ++ mergee (xs) ([y] ++ ys)

msort :: Ord a => [a] -> [a]
msort [a] = [a]
msort a = mergee (msort (fst (halve a)))  (msort (snd (halve a)))

tupleToList :: ([a],[a]) -> [[a]]
tupleToList (l1,l2) = [l1] ++ [l2]

halve :: [a] -> ([a],[a]) 
halve a = halveHelp a ([],[])

halveHelp :: [a] -> ([a],[a]) -> ([a],[a])
halveHelp [] (l1,l2) = (l1,l2)
halveHelp [a] (l1,l2) = (l1,l2 ++ [a])
halveHelp a (l1,l2)
    | (length a) > length l1 = halveHelp (tail a) (l1 ++ [head a], l2)
    | otherwise = halveHelp (tail a) (l1, l2 ++ [head a])

--main = print(msort [1,5,0,3,8,7,2,9,20,0,17,11,4])
main = print(map (+2) [1,2,3,4,5])