module Week38Exercise2 where

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

fromJust :: Maybe a -> a
fromJust (Just a) = a


listComp :: [Maybe a] -> [a]
listComp a = [fromJust x | x <- a, not (isNothing x)]

--main = print(isNothing (Just 5))
--main = print(listComp [Just 1, Nothing, Nothing, Just 3, Nothing])

removeNothing :: [Maybe a] -> [a]
removeNothing a = removeNothingHelp a []

removeNothingHelp :: [Maybe a] -> [a] -> [a]
removeNothingHelp (x:xs) y
    | not (isNothing x) = removeNothingHelp xs y
    | otherwise = removeNothingHelp xs ([fromJust x] ++ y)


main = print(removeNothing [Just 1, Nothing, Nothing, Just 3, Nothing])
