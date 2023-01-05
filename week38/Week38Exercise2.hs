module Week38Exercise2 where

removeNothing :: [Maybe a] -> [a]
removeNothing a = removeNothingHelp a []

removeNothingHelp :: [Maybe a] -> [a] -> [a]
removeNothingHelp [] y = y
removeNothingHelp (Just x: xs) y = removeNothingHelp xs (y ++ [x])
removeNothingHelp (Nothing : xs) y = removeNothingHelp xs y


main = print(removeNothing [Just 1, Just 3, Nothing, Just 5, Just 7, Just 10])
