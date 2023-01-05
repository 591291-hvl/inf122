import Data.Char 

main :: IO ()
main = 
    getLine >>= \a ->
    putStrLn $ helper a

-- main :: IO ()
-- main = do
--     a <- getLine
--     putStrLn $ helper a

helper ::String -> String
helper a = [x | x <- a, isUpper x]
