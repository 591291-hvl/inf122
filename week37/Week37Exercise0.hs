module Week37Exercise0 where

namesAndAges :: [String] -> [Integer] -> [String]
namesAndAges names ages = [(fst x) ++ " is " ++ show (snd x) ++ " years old" | x <- (filter ((<=50).snd) (zip names ages))]
