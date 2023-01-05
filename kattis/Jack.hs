main :: IO ()
main = 
    getLine >>= \a ->
    print $ helper $ map read $ words a

-- main :: IO ()
-- main = do
--     a <- getLine
--     print $ helper $ map read $ words a

helper :: [Int] -> Int
helper [x,y,z] = (x*y*z)
