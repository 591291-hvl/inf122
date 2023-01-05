data Nat = Zero | Succ Nat
    deriving (Eq, Show)


nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

addd :: Nat -> Nat -> Nat
addd Zero n = n
addd (Succ m) n = Succ (addd m n)

mult :: Nat -> Nat -> Nat
mult n1 Zero = n1
mult n1 (Succ n2) = addd n1 (mult n1 n2)

--main = print(nat2int $ mult (int2nat 3) (int2nat 1))



data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Eq, Show)

balanced :: Tree a -> Bool
balanced (Leaf a) = True
balanced (Node left right) = if 2 < (abs ((height left) - (height right))) then False else (balanced left) && (balanced right)


height :: Tree a -> Integer
height (Leaf a) = 1
height (Node left right) = 1 + (max (height left) (height right))

-- main = print $ balanced 
--     (Node 
--         (Node 
--             (Node 
--                 (Node 
--                     (Leaf 1)
--                     (Leaf 1)) 
--                 (Leaf 1)) 
--             (Node 
--                 (Leaf 1) 
--                 (Leaf 1)))
--         (Leaf 1))
-- main = print(balanced (Node  (Leaf 1) (Leaf 1)))


balance :: [a] -> Tree a
balance [elm] = (Leaf elm)
balance list =  (\(left, right) -> (Node (balance left) (balance right))) (split list)

split :: [a] -> ([a],[a])
split list = (take n list, drop n list)
    where n = (div (1 + length list) 2)

-- main = print(balance [1,2,3,4,5,6])



data Expr = Val Int | Add Expr Expr
    deriving (Eq, Show)


folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val elm) = f elm
folde f g (Add elm1 elm2) = g (folde f g elm1) (folde f g elm2) 
 

-- main = print(folde abs (+) (Add (Val 3) (Val 2)))

eval :: Expr -> Int
eval exp = folde (\x -> x) (\x y -> x + y) exp

size :: Expr -> Int
size exp = folde (\x -> 1) (\x y ->  x + y) exp

-- main = print $ eval (Add (Val 3) (Val 2))

instance Eq a => Eq (Maybe a) where
    _ = Nothing
    a = Just a
    


instance Eq a => Eq [a] where
    _ = []
    a = [a]


