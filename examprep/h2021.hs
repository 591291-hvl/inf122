-- 1.1

takeW :: Int -> String -> String
takeW n str = unwords $ take n (words str)

-- 1.2
erSubListe :: (Eq a) => [a] -> [a] -> Bool
erSubListe _ [] = False
erSubListe [] _ = True
erSubListe (x:xs) (y:ys) = case x == y of
    True -> erSubListe xs ys
    False -> erSubListe (x:xs) ys

erSubstreng :: (Eq a) => [a] -> [a] -> Bool
erSubstreng [] _ = True
erSubstreng (x:xs) (y:ys) = case x == y of
    True -> erSubstreng xs ys
    False -> False


-- 2.1
finnFiksPunkt :: (Eq a) => (a->a) -> a -> Int -> Maybe a
finnFiksPunkt f x 0 = Nothing
finnFiksPunkt f x n = case f x == x of
    True -> Just x
    False -> finnFiksPunkt f (f x) (n-1)

-- 2.2
serie :: (Int -> b) -> Int -> [b]
serie f i = f i : serie f (i+1)

-- 2.3
sq :: Int -> [Int]
sq n = take n (serie (^2) 0)

streng :: Int -> String
streng n = init $ concat $ take n (serie (\x -> "f(" ++ (show x)++"),") 0)

-- 2.4
h :: (Int -> Int) -> Int -> [Int]
h f i = hHelper f i 1

hHelper :: (Int -> Int) -> Int -> Int -> [Int]
hHelper f i counter = (sum $ take counter (serie f i)) : hHelper f (i) (counter+1)

-- 2.5
-- hva xs = foldr (++) [] (map sing xs) where sing x = [x]
-- map sing xs, legger alle element inn i en liste [[1],[2],[3]]
-- foldr (++) [] xs, slår sammen listen igjen, så man får [1,2,3]

-- 2.6
-- map ?1 . filter ?2 = filter (>0) . map (+1)
-- ekvalent funksjon vil være:
-- map (+1) . filter (>(-1))

-- main = print $ (filter (>0) . map (+1)) [(-5)..5]
-- main = print $ (map (+1) . filter (>(-1))) [(-5)..5]


data Tre = Nd Int [Tre] deriving (Eq, Show)
-- 3.1
antb :: Tre -> Int
antb (Nd t []) = 1
antb (Nd t trs) = sum $ map (\x -> antb x) trs

-- main = print $ antb (Nd 1 [(Nd 2 []), (Nd 3 []), (Nd 4 [])])

-- 3.2
stil :: Tre -> Int
stil tre = stilHelper tre 0

stilHelper :: Tre -> Int -> Int
stilHelper (Nd t []) acc = 1
stilHelper (Nd t trs) acc = acc + ((sum $ map (\x -> stilHelper x (acc+1)) trs)+1)


t = Nd 1 [Nd 2 [Nd 3 []], Nd 4 [], Nd 5 [Nd 6 [], Nd 7 []]]

-- main = print $ stil t

-- 3.3
kup :: Tre -> Tre
kup (Nd t []) = Nd t []
kup (Nd t trs) = Nd (t*(product $ map (\(Nd x _) -> x) xs)) xs
    where xs = map kup trs 

kus :: Tre -> Tre
kus (Nd t []) = Nd t []
kus (Nd t trs) = Nd (t+(sum $ map (\(Nd x _) -> x) xs)) xs
    where xs = map kus trs 

-- main = print $ kus $ kup t


-- 3.4
toStr :: Tre -> String
toStr (Nd t []) = (show t) ++ "-"
toStr (Nd t trs) = (show t) ++ "," ++ (concat $ map toStr trs)++ "-"

-- 3.5
bladene :: String -> [Int]
bladene str = bladeneHelper str "" []

bladeneHelper :: String -> String -> [Int] -> [Int]
bladeneHelper [] holder acc = acc
bladeneHelper (x:str) holder acc
    | x == ',' = bladeneHelper str "" acc
    | x == '-' = bladeneHelper str "" (if length holder == 0 then acc else (acc ++ [read holder]))
    | otherwise = bladeneHelper str (holder ++ [x]) acc


-- main = print $ bladene $ toStr t

main = print $ f = map snd . filter (odd . fst) . zip [0..]
