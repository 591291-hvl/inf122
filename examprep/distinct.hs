
distinct :: Eq a => [a] -> [a] -> [a]
distinct [] acc = acc
distinct (x:xs) acc = case elem x acc of
    True -> distinct (xs) acc
    False -> distinct (xs) (acc++x:[]) 


-- main = print $ distinct [1,3,2,1,4,5,3] []

main = putStrLn $ distinct "aabbcbe" ""