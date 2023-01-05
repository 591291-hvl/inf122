module ExtraExercise2 where

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)

instance Functor BinSearchTree where
  fmap :: (a -> b ) -> BinSearchTree a -> BinSearchTree b
  fmap f Empty = Empty
  fmap f (Branch left elm right) = (Branch (fmap f left) (f elm) (fmap f right))

tree = (Branch (Branch Empty 1 Empty) 2 (Branch Empty 3 Empty))

main = print $ fmap (+2) tree
