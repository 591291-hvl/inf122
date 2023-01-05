-- 1.1

abc :: t -> Int -> t -> [t]
abc x n y = (take (n) (repeat x)) ++ [y]

-- main = print $ abc 1 2 3 
-- main = print $ abc 'a' 5 'b'

-- 1.2

tryggIndeks :: [t] -> Int -> t -> t
tryggIndeks xs n def = case length xs > n of
    True -> xs !! n
    False -> def

-- main = print $ tryggIndeks "abcd" 6 'x'

-- 1.3

inds :: String -> String -> [Int]
inds sub str = indsHelper sub str 0 []

indsHelper :: String -> String -> Int -> [Int] -> [Int]
indsHelper sub str counter acc 
    | length sub > length str = acc
    | otherwise = if take (length sub) str == sub
        then indsHelper sub (drop 1 str) (counter+1) (acc++[counter])
        else indsHelper sub (drop 1 str) (counter+1) (acc)

-- main = print $ inds "ab" "xabaabbab"


-- 1.4

ordTeller :: String -> [(String, Int)]
ordTeller input = unik [(x,teller x list) | x <- list] []
    where list = sort (words input)

teller :: String -> [String] -> Int
teller x = length . filter (==x) 

unik :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
unik [] acc = acc
unik (x:xs) acc = case elem x acc of
    True -> unik xs acc
    False -> unik xs (acc++[x])

sort :: [String] -> [String]
sort [] = []
sort (x:xs) = (sort left) ++ [x] ++ (sort right)
    where 
        left = filter (<x) xs
        right = filter (>=x) xs
        


-- main = print $ ordTeller "ikke alle er her er der alle men ikke er"


-- 1.5

dec :: Int -> String -> Int
dec nmb str = decHelper nmb str 0

decHelper :: Int -> String -> Int -> Int
decHelper nmb [] acc = acc
decHelper nmb (x:str) acc 
    | nmb <= (read [x]) = 0
    | otherwise = decHelper nmb (str) (acc + (read [x])*(nmb^((length str))))

-- main = print $ dec 4 "301"


type EtOrd = String
type Linje = [EtOrd]
type Side = [Linje]
type Bok = [Side]
-- 2.1
antLinjer :: Bok -> Int
antLinjer bok = sum $ map (\x -> length x) bok

antOrd :: Bok -> Int 
antOrd bok = sum $ map (\x -> sum $ map (\y -> length y) x) bok

-- 2.2
eqs :: Bok -> Bok -> Bool
eqs bok1 bok2 = (concat $ concat $ concat bok1) == (concat $ concat $ concat bok2)

-- main = print $ eqs [[["dette","er","forste linje"],["pa","side","en"]],[],[["siste","linje"]]] [[["dette","er","forste linje"]],[["pa","side","en"],["siste","linje"],[]]]


data Tre t = Nd t [Tre t] deriving(Eq,Show)
-- 3.1

noder :: Tre t -> [t]
noder (Nd t []) = [t]
noder (Nd t trs) = [t] ++ concat (map noder trs)

hoyde :: Tre t -> Int
hoyde (Nd t []) = 1
hoyde (Nd t trs) = 1 + maximum (map hoyde trs)


-- 3.2
iso :: (Tre a, Tre b) -> Bool
iso (Nd t1 [], Nd t2 []) = True
iso (Nd t1 (x:trs1), Nd t2 (y:trs2)) = case (length trs1) == (length trs2) of
    True -> True && ((iso (x,y) == True) && iso (Nd t1 trs1, Nd t2 trs2) == True)
    False -> False

t1 = Nd 1 [Nd 2 [Nd 3 []], Nd 4 [], Nd 5 [Nd 6 []]]
t2 = Nd 1 [Nd 2 [Nd 3 []], Nd 4 [], Nd 5 [Nd 6 [], Nd 7 []]]

-- main = print $ iso (t1, t2) 


-- 3.3

instance Functor Tre where
    fmap :: (a->b) -> Tre a -> Tre b
    fmap f (Nd t []) = (Nd (f t) [])
    fmap f (Nd t trs) = (Nd (f t) (map (fmap f) trs)) 

up :: Tre Int -> Tre Int
up tre = fmap (1+) tre

constt :: Tre t -> String -> Tre String
constt tre str = fmap (\x -> str) tre

-- main = print $ constt t1 "c"


-- 3.4

inns :: Eq t => t -> Tre t -> Tre t -> Tre t
inns n (Nd t trs) t2 = case t == n of
    True -> (Nd t (trs++[t2]))
    False -> (Nd t (map (\x -> inns n x t2) trs)) 

main = print $ inns 1 (Nd 1 [Nd 2 [Nd 3 []], Nd 4 []]) (Nd 5[Nd 6[], Nd 7[]])