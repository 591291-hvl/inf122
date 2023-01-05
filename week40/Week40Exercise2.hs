module Week40Exercise2 where

data BinSearchTree a
    = Empty
    | Branch (BinSearchTree a) a (BinSearchTree a)
    deriving (Eq, Show)

t = Branch (Branch (Branch Empty 1 Empty) 3 (Branch Empty 5 Empty)) 8 (Branch Empty 10 Empty)
t1 = Empty

toList :: BinSearchTree a -> [a]
toList Empty = []
toList (Branch left elm right) = createList (Branch left elm right) []

createList :: BinSearchTree a -> [a] -> [a]
createList Empty listt = listt
createList (Branch left elm right) listt = listt ++ (createList left []) ++ [elm] ++ (createList right [])
