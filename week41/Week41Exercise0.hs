module Week41Exercise0 where

import Data.Map (Map) 
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set




-- data Point a = Pt a a
--     deriving (Eq, Show)

-- p1 = (Pt "a" "a")

-- main = print(p1)

type Graph n = Map n (Set n)


s :: Graph Integer
s = Map.fromList [(1 , Set.fromList [1]) ,(3, Set.fromList [3])]

s1 :: Graph Integer
s1 = Map.fromList []

insertEdge :: (Ord n) => n -> n -> Graph n -> Graph n
insertEdge n1 n2 graph =  Map.insertWith Set.union n2 Set.empty (Map.insertWith Set.union n1 (Set.singleton n2) graph)
        

-- s2 = Map.insertWith Set.union n2 (Set.singleton n1) s1
main = print $ Map.insert 1 Set.empty s1

-- main = print $ insertEdge 1 2 s1

--main = print(Map.insertWith 1 4 s1)
