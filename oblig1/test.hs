
import qualified Data.Map as Map
import Data.Map (Map)
import System.Random
import Data.List

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Codec.Compression.GZip as GZip

import System.IO
import System.Environment

gramLen :: (Num a) => a
gramLen = 7

n = 3
s = "Haskell"


type NGram = String
type Weight = Integer

grams :: Integer -> String -> [NGram]
grams n s 
    | x > length s = []
    | otherwise = (take x s) : grams n (tail s)
    where x = (fromIntegral n)

gramsWithNext :: Integer -> String -> [(NGram,Char)]
gramsWithNext n s = zip (grams n s) (map last(tail(grams n s)))

combineGrams :: [NGram] -> String
combineGrams [] = []
combineGrams [x] = x
combineGrams (x:xs) = (head x) : combineGrams xs

-- main = print $ combineGrams ["PC","C^","^+","+t"]

-- main = print $ (grams 3 "Haskell")
-- main = print $ combineGrams (grams 3 "Haskell")
-- main = print $ combineGrams []


-- The type for our Markov process text model.
type TextModel = Map NGram (Map Char Weight , Weight)

emptyModel :: TextModel
emptyModel = Map.empty

increaseWeight :: NGram -> Char -> TextModel -> TextModel
increaseWeight ngram next model
    |model == emptyModel = Map.fromList [(ngram,(Map.fromList [(next,1)],1))]
    |Map.lookup ngram model == Nothing = Map.insert ngram (Map.fromList [(next,1)],1) model
    |otherwise = Map.insert ngram ((\ (Just x) -> Map.insertWith (+) next (toInteger 1) (fst x)) (Map.lookup ngram model), sum $ Map.elems ((\ (Just x) -> Map.insertWith (+) next (toInteger 1) (fst x)) (Map.lookup ngram model))) model
        where insidemap = (\ (Just x) -> Map.insertWith (+) next (toInteger 1) (fst x)) (Map.lookup ngram model)

-- t :: TextModel
-- t = Map.fromList [("det",(Map.fromList [(' ',1),('t',1)],2))]

-- m0 = increaseWeight "det" 't' emptyModel
-- m1 = increaseWeight "det" 't' m0
-- main = print $ increaseWeight "det" ' ' m1



-- createModel :: Integer -> String -> TextModel
-- createModel n string = foldl' (gramsWithNext n string) emptyModel

-- foldl' :: [(NGram, Char)] -> TextModel -> TextModel
-- foldl' [] model = model
-- foldl' ((x,y):xs) model = increaseWeight x y (foldl' xs model)

-- main = print $ foldl' (+) 0 [1..10]

-- t :: TextModel
-- t = emptyModel

-- main = print $ (gramsWithNext 2 "string") 
-- main = print $ foldl' (\x n -> increaseWeight (fst x) (snd x) n) (gramsWithNext 2 "string") emptyModel

helpfunc :: TextModel -> (NGram,Char) -> TextModel
helpfunc model (gram,char) = increaseWeight gram char model

createModel :: Integer -> String -> TextModel 
createModel n string = foldl' helpfunc emptyModel (gramsWithNext n string)



-- main = print $ createModel 2 "ba"


nextDistribution :: TextModel -> NGram -> Maybe ([(NGram, Weight)],Weight)
nextDistribution model current
    |Map.lookup current model == Nothing = Nothing
    |otherwise = (\(Just (a,b)) -> Just (map (\(x,y) -> ((tail current ++ [x]),y)) (Map.toList a),b)) (Map.lookup current model)


main = print $ nextDistribution (createModel 3 "Petter setter dette rett etter.") "ett"


pick :: [(a,Weight)] -> Weight -> a
pick [weight] treshold = fst weight
pick (weight:weights) treshold 
    | (snd weight) > treshold = fst weight
    | otherwise = pick weights (treshold - (snd weight))


-- main = print $ map (pick $ zip "abcde" [1..]) [0..10]
--"abbcccdddde"

-- main = print $ map (pick [("aa",2),("ab",3)]) [0..4]

pickRandom :: [(a,Weight)] -> Weight -> IO a
pickRandom wl total = do
    num <- randomRIO (0, total-1)
    return (pick wl num)

-- main = do 
--     x <- pickRandom [("aa",1),("ab",9)] 10
--     print $  x



generate' :: TextModel -> NGram -> Integer -> IO [NGram]
generate' model start 0 = do return []
generate' model start amount = if gramList == Nothing
    then do return []
    else do
        a <- ((\(Just x) -> pickRandom (fst x) (snd x)) $ gramList)
        b <- generate' model a (amount-1)
        return (a : b)
        where gramList = nextDistribution model start


        

m = createModel 7 "aaaaaaaabbbbbbbb"

m1 = createModel 7 "Petter setter dette rett etter."

-- main = print $ m

-- main = print ((\(Just x) -> x) $ nextDistribution m "aaabbbb")


-- main = do
--     a <-  generate' m1 "Petter " gramLen
--     print $ a


generate :: TextModel -> String -> Integer -> IO String
generate model start amount =
    do
        a <-  (generate' model (last (grams gramLen start)) (amount))
        return (take(length start - gramLen + 1) start ++ take (fromInteger amount + gramLen - length start - 1)(combineGrams a))
        -- return $ take (fromIntegral amount) (start ++ combineGrams a)



-- main = do
--     x <- generate' m1 (last $ grams gramLen "setter ") 12
--     print $ fmap head $ x



-- main = do
--     x <- generate m "aaabbbb" 14
--     print x


readModel :: Handle -> IO TextModel
readModel h = do
    a <- ByteString.hGetContents h
    let b = read $ UTF8.toString $ GZip.decompress a
    return b


