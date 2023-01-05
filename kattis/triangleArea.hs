main :: IO ()
main =
    getLine >>= \a ->
    print $ helper $ map read $ words a

-- main :: IO ()
-- main = do
--     a <- getLine
--     print $ helper $ map read $ words a

helper ::(Floating a) => [a] -> a
helper [x,y] = (x*y) / 2

