main :: IO ()
main =
    getLine >>= \a ->
    putStrLn (case mod (read a) 2 == 0 of {True -> "Bob"; False -> "Alice"})

-- main :: IO ()
-- main = do
--     a <- getLine
--     putStrLn (case mod (read a) 2 == 0 of {True -> "Bob"; False -> "Alice"})