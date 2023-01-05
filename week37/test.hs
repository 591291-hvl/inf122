splitHalf :: [Integer] -> [[Integer]]
splitHalf list = [take (length list `div` 2) list] ++ [(reverse $ take (length list `div` 2) (reverse list))]

splitHalf1 :: [Int] -> [[Int]]
splitHalf1 l = [take hlen l, drop hlen l]
    where hlen = div (length l) 2

third :: [a] -> a
third list = head $ tail $ tail list

third1 :: [a] -> a
third1 list = list !! 2

third2 :: [a] -> a
third2 (a:b:c:_) = c


safetail :: [a] -> [a]
safetail a = if null a then [] else tail a

safetail1 ::[a] -> [a]
safetail1 a | null a = []
            | otherwise = tail a

safetail2 ::[a] -> [a]
safetail2 [] = []
safetail2 (a:as) = as

squared :: Integer -> [Integer]
squared n = [x * x | x <- [1..n]]

(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False


powerOfTo100 :: [Integer]
powerOfTo100 = [x^2 | x <- [1..100]]

grid :: Integer -> Integer -> [(Integer, Integer)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n] ]

square :: Integer -> [(Integer, Integer)]
square x = [ (a,b) | (a,b) <- (grid x x),  a /= b]

replicate1 :: Int -> a -> [a]
replicate1 numb a = take numb (cycle [a])

replicate2 :: Int -> a -> [a]
replicate2 numb a = [a | x <- [1..numb]]

pyths :: Int -> [(Int, Int, Int)]
pyths a = [(x,y,z) | x <- [1..a], y <- [1..a], z <- [1..a], x*x + y*y == z*z]

factors :: Int -> [Int]
factors n = [x | x <- [1..(n-1)], n `mod` x == 0]

perfect :: Int -> [Int]
perfect n = [x | x <- [1..n], sum (factors x) == x]

twoComprehensions :: [(Int,Int)]
twoComprehensions = concat[[(x,y) | y <- [3,4]] | x <- [1,2]] 

newTake :: Integer -> [a] -> [a]
newTake x [] = []
newTake x (a:as) 
    | x == 0 = []
    | otherwise = a : newTake (x-1) as

main = print(newTake 4 [0,1,2,3,4,5,6])
--main = print(take 3 (cycle ["a"]))