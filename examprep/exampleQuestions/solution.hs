import Data.Map (Map)
import qualified Data.Map as Map



-- Regne/flervalgsoppgaver

-- a) Hva er verdien til uttrykket map (+3) [1,2,3]?

-- -> [4,5,6]

-- b) Hva er verdien til uttrykket sum [x+3 | x <- [1,2,3]]?

-- -> [4,5,6]

-- c) Hva er verdien til uttrykket (\x y -> x-y) 7 3 ?

-- -> 4

-- d) Hva er typen til uttrykket (\x -> 3 : tail x)?

-- -> [Integer]

-- e) Hva er riktig type til uttrykket (3,Just "Haskell")?

-- -> (Integer, Maybe String)

-- f) Hva er riktig type til uttrykket Just (Left Nothing)?

-- -> Maybe (Either (Maybe a) b)

-- g) Hvilken kind har Either String?

-- -> * -> *


-- Enkel IO
-- Skriv et program som leser inn et navn på formen “Fornavn Etternavn” og
-- returner “Etternavn, Fornavn”.

-- main :: IO ()
-- main = do
--     line <- getLine
--     putStrLn $ (last (words line)) ++ ", " ++ (head (words line))


-- Listeoperasjoner

-- a) Skriv en funskjon isConsonant :: Char -> Bool som sjekker om en char
-- er en konsonant på norsk.

isConsonant :: Char -> Bool 
isConsonant char = elem char "bcdfghjklmnpqrstvwxz"


-- b) Funksjonen translate bruker listekomprehensjon, skriv den slik at den
-- bruker map istedet.

-- translate :: String -> String
-- translate word = concat $ map (\x -> if isConsonant x then [x,'o',x] else [x] ) word


-- c) Skriv funksjonen translate slik at den bruker do-notasjon for lister.

translate :: String -> String
translate string = do
    chars <- string
    if isConsonant chars 
        then chars : 'o' : [chars] 
        else [chars]

translate1 :: String -> String
translate1 string = string >>= \chars -> 
    if isConsonant chars 
        then chars : 'o' : [chars] 
        else [chars]

translate2 :: String -> String
translate2 string = concatMap  (\chars -> 
    if isConsonant chars 
        then chars : 'o' : [chars] 
        else [chars]) string

-- d) Skriv en funksjon differences :: [Integer] -> [Integer] som regner
-- ut alle positive differanser mellom elementene i en liste, ved hjelp av listekomprehensjon. Eksempel differences [1,2,3] = [1,2,1], fordi
-- 2-1 = 1 og 3-1 = 2 og 3-2 = 1.


differences :: [Integer] -> [Integer] 
differences list = [x-y | x <- list, y <- list , x > y]


-- e) Skriv en funskjon, everyOther :: [a] -> [a], som fjerner annethvert
-- element fra en liste. Behold det første elementet i listen, fjern det andre,
-- behold det tredje osv. Eksempel: everyOther [1,2,3,4] = [1,3].


everyOther :: [a] -> [a]
everyOther list = everyOtherHelper list True []

everyOtherHelper :: [a] -> Bool -> [a] -> [a]
everyOtherHelper [] _ acc = acc
everyOtherHelper (x:xs) True acc = everyOtherHelper xs False (acc++[x])
everyOtherHelper (x:xs) False acc = everyOtherHelper xs True (acc)


-- Map


type Graph label node = Map node (Map label node)

-- data N = A | B | C
--     deriving(Show, Eq)

graph0 :: Graph Char Int
graph0 = Map.fromList [(1,Map.fromList [('r',2)]),(2,Map.fromList [('o',2),('t',3)]),(3,Map.fromList [('e',1),('t',3)])]

graph1 :: Graph Char Int
graph1 = Map.fromList [(1,Map.fromList[('a',2)])]

-- a) Skriv en funksjon som setter inn en kant med en gitt merkelapp mellom to
-- noder i en graf.

insertLabeledEdge ::(Ord label, Ord node) => Graph label node -> node -> node -> label -> Graph label node
insertLabeledEdge graph n1 n2 l = Map.insertWith Map.union n1 (Map.singleton l n2) graph

-- main = putStrLn $ show $ insertLabeledEdge graph1 1 3 'b'



-- b) Bruk do-notasjon for Maybe til å skrive en funksjon som slår opp en node
-- og en label i en graf og gir den neste noden:

goNext ::(Ord label, Ord node) => Graph label node -> node -> label -> Maybe node 
goNext graph start label = do
    a <- (Map.lookup start graph)
    b <- (Map.lookup label a)
    return b

-- c) Skriv goNext ved hjelp av (»=)-operatoren istedet for do-notasjon.


goNext1 ::(Ord label, Ord node) => Graph label node -> node -> label -> Maybe node 
goNext1 graph start label =
    (Map.lookup start graph) >>= \a ->
    (Map.lookup label a) >>= \b ->
    return b


-- d) Skriv en rekursiv funksjon som følger en liste med labels fra en start node
-- til en sluttnode.

followPath :: (Ord label, Ord node) => Graph label node -> node -> [label] -> Maybe node
followPath graph node [] = return node
followPath graph node label = do
    nextNode <- goNext graph node (head label)
    followPath graph nextNode (tail label)


-- main = putStrLn $ show $ followPath graph0 1 "rotte"



-- foldl vs foldr


-- a) Bruk definisjonen til å regne ut: foldr (:) [] [1,2,3].


-- foldr (:) [] [1,2,3]
-- (:) 1(foldr (:) [] [2,3])
-- (:) 1 ((:) 2 (foldr (:) [] [3]))
-- (:) 1 ((:) 2 ((:) 3 (foldr (:) [] [])))
-- (:) 1 ((:) 2 ((:) 3 []))


-- b) Bruk definisjonen til å regne ut: foldl (\l a -> a:l) [] [1,2,3]

-- foldl (\l a -> a:l) [] [1,2,3]
-- foldl (\l a -> a:l) ((\l a -> a:l) [] 1) [2,3]
-- foldl (\l a -> a:l) ((\l a -> a:l) ((\l a -> a:l) [] 1) 2) [3]
-- foldl (\l a -> a:l) ((\l a -> a:l) ((\l a -> a:l) ((\l a -> a:l) [] 1) 2) 3) []
-- ((\l a -> a:l) ((\l a -> a:l) ((\l a -> a:l) [] 1) 2) 3)


-- c) Forklar hva som skjer hvis man evaluerer følgende uttrykk:
-- • and (repeat False)

-- repeat lager en evig liste med False
-- and bruker foldr til å gjøre sammenligning på en liste
-- første itersjon vil bli:
-- foldr (&&) True ([repeat False])
-- (&&) False (foldr (&&) True (repeat False))
-- Siden haskell er lazy regner den kun ut det den trenger:
-- (&&) False (noe) -> False


-- • and' (repeat False)
-- and' bruker foldl, som bygger utrykker innenfra og ut
-- første iterasjon vil bli:
-- foldl (&&) True (repeat False)
-- foldl (&&) ((&&) True False) (repeat False)
-- siden haskell er lazy så vil den prøve å løse utrykket på utsiden, 
-- utrykket på utsiden er repeterende med samme lengde på listen, altså evig.. kommer aldri til å kunne løse den


main = print $ foldl (&&) ((&&) True False) (repeat False)
