{-# LANGUAGE InstanceSigs #-}
module Week43Exercise1 where

data RoseTree a = Branch a [RoseTree a]
    deriving (Eq, Show)

t = Branch 1 [Branch 2 [], Branch 3 []]


roseTreeToList :: RoseTree a -> [a]
roseTreeToList (Branch x []) = [x]
roseTreeToList (Branch x list) = [x] ++ concat (map roseTreeToList list)

instance Functor RoseTree where
    fmap :: (a -> b) -> RoseTree a -> RoseTree b
    fmap f (Branch elm []) = (Branch (f elm) [])
    fmap f (Branch elm list) = (Branch (f elm) (map (fmap f) list))

sumNodes :: (Num a) => RoseTree [a] -> RoseTree a
sumNodes tree = fmap sum tree

rt = Branch [-1,1,-1] [Branch [1,-1] [Branch [] []]]

main = print $ sumNodes rt