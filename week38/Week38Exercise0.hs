module Week38Exercise0 where

duplicateAll :: [a] -> [(a,a)]
duplicateAll [] = []
duplicateAll [a] = [(a,a)]
duplicateAll (a:as) = [(a,a)] ++ duplicateAll as

main = print(duplicateAll [1,2,3])