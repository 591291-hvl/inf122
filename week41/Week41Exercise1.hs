module Week41Exercise1 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)


disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint s1 s2 = Set.null (Set.intersection s1 s2)
-- disjoint s1 s2 = Set.intersection s1 s2 

s1 = Set.fromList[1,2]
s2 = Set.fromList[4,5]

-- main = print(disjoint s1 s2)

trueCycle :: (Ord n) => Graph n -> n -> Bool
trueCycle graph node = trueDoCycle graph node node Set.empty

trueDoCycle :: (Ord n) => Graph n -> n -> n -> Set n -> Bool
trueDoCycle graph start node set = 
    if start == node && Set.member start set
    then True 
    else
        if disjoint (Set.delete start set) (neighbors graph node) 
        then or $ map (\x -> trueDoCycle graph start x (Set.insert node set)) (Set.toList $ neighbors graph node)
        else False


hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle graph node = doCycle graph node Set.empty

doCycle graph node set = 
    if not (disjoint set (neighbors graph node))
    then True
    else 
        if Set.null (neighbors graph node)
        then False 
        else or $ map (\x -> doCycle graph x (Set.insert node set)) (Set.toList $ neighbors graph node)

neighbors :: (Ord n) => Graph n -> n -> Set n
neighbors graph node = (\(Just x) -> x) $ Map.lookup node graph

s :: Graph Integer
s = Map.fromList [
    (1, Set.fromList [2]),
    (2, Set.fromList [3]), 
    (3, Set.fromList [3,4]),
    (4, Set.fromList [])]

main = print(hasCycle s 3)