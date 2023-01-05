main :: IO ()
main = 
    getLine >>= \a ->
    print $ helper $ words a

-- main :: IO ()
-- main = do
--     a <- getLine
--     print $ helper $ words a

helper :: [String] -> Int
helper [x,y] = case x > y of
    True -> 1
    False -> 0
