module Week42Exercise1 where
import Data.Either

fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
fromLeftAndRight f = (\a -> f (Left a),\b -> f (Right b))

banan :: Either bool Integer -> String
banan (Left b) = "Det er en bool!"
banan (Right i) = "Det er en Integer!" ++ show i

-- main = do
--     let (a, b) = fromLeftAndRight banan
--     print (a True)
--     print (b 1)

either' :: (a -> c) -> (b -> c) -> Either a b -> c 
either' f _ (Left x) = f x
either' _ g (Right y) = g y

-- s = Left "foo" :: Either String Int
n = Right 3 :: Either String Int

-- main = print $ either' length (*2) s


toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
toFstAndSnd func = (fst . func, snd . func)

pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair f1 f2 elm = (f1 elm, f2 elm)

-- a :: Either Int Int 
-- a = Left 2

-- a1 :: Either (Int -> Int) (Int -> Int) 
-- a1 = Left (+2)

-- a2 :: Either x y -> z
-- a2 = Left even

-- main = print $ a2

-- main = print $ (\(Left x) -> x 1) a1
-- main = print $ (fromLeft 0 a)
-- main = print $ fromLeftAndRight a2