{-# LANGUAGE FlexibleInstances #-}
module Week43Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

class IntegerGraph g where
    emptyGraph :: g
    insertNode :: Integer -> g -> g
    insertEdge :: Integer -> Integer -> g -> g
    nodeInGraph :: Integer -> g -> Bool
    edgeInGraph :: Integer -> Integer -> g -> Bool

newtype MyGraph = MyGraph (Map Integer (Set Integer))
    deriving(Show)




instance IntegerGraph MyGraph where
    emptyGraph = MyGraph (Map.fromList [])
    insertNode n g = MyGraph (Map.insert n Set.empty $ (\(MyGraph x) -> x ) g)
    insertEdge n1 n2 g = MyGraph (Map.insertWith Set.union n2 Set.empty (Map.insertWith Set.union n1 (Set.singleton n2) $ (\(MyGraph x) -> x ) g))
    nodeInGraph n g = Map.member n $ (\(MyGraph x) -> x ) g
    edgeInGraph n1 n2 g = if not (nodeInGraph n1 g)
                            then False
                            else  (\(Just x ) -> Set.member n2 x ) (Map.lookup n1 $ (\(MyGraph x) -> x ) g)

g :: MyGraph
g = MyGraph (Map.fromList [(1 , Set.fromList [1,3]) ,(3, Set.fromList [3])])

graph :: (IntegerGraph g) => g
graph = insertEdge 8 5 $ insertEdge 5 8 $ insertEdge 5 1 $ insertEdge 1 8 $ insertEdge 1 6 $ insertNode 8 $ insertNode 6 $ insertNode 5 $ insertNode 3 $ insertNode 1 emptyGraph



main = print $ graph