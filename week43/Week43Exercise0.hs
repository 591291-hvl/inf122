{-# LANGUAGE InstanceSigs #-}
module Week43Exercise0 where

data BinSearchTree a
    = Empty
    | Branch (BinSearchTree a) a (BinSearchTree a)
    deriving (Eq, Show)


t = Branch (Branch (Branch Empty 1 Empty) 3 (Branch Empty 5 Empty)) 8 (Branch Empty 10 Empty)

-- TEST
-- toListRec :: BinSearchTree a -> [a]
-- toListRec Empty = []
-- toListRec (Branch Empty elm Empty) = [elm]
-- toListRec (Branch (left) elm (right)) = (toListRec left) ++ [elm] ++ (toListRec right)
-- TEST


-- instance Foldable [] where
--     foldr :: (a -> b -> b) -> b -> [a] -> b
--     foldr _ z []     =  z
--     foldr f z (x:xs) =  x `f` foldr f z xs

instance Foldable BinSearchTree where
    foldr :: (a -> b -> b) -> b -> BinSearchTree a -> b
    foldr _ z Empty = z
    foldr f z (Branch (left) elm (right)) = foldr f (elm `f` (foldr f z right)) left


toList :: BinSearchTree a -> [a]
toList tree = foldr (:) [] tree

main = print $ toList t 