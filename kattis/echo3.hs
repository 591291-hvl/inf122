main :: IO ()
main = 
    getLine >>= \a ->
    putStrLn $ helper a

-- main :: IO ()
-- main = do
--     a <- getLine
--     putStrLn $ helper a

helper :: String -> String
helper a = unwords $ take 3 (repeat a)
