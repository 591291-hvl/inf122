module ExtraExercise1 where

takeMaybe :: Integer -> [a] -> Maybe [a]
takeMaybe n l 
    | n == 0 = Just []
    | n < 0 = Nothing
    | n > toInteger (length l) = Nothing
    | otherwise = Just (take (fromInteger n) l)

main = print $ takeMaybe 2 [1,2,3,4]