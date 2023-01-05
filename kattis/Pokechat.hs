main :: IO()
main = 
    getLine >>= \str ->
    getLine >>= \code ->
    putStrLn $ helper str code ""

helper:: String -> String -> String -> String
helper str [] builder = builder
helper str (x:y:z:code) builder = helper str (code) (builder ++ [(str !!((read $ x:y:z:"" :: Int) -1))])


-- main :: IO()
-- main = do
--     str <- getLine
--     code <- getLine
--     putStrLn $ helper str code ""

-- helper:: String -> String -> String -> String
-- helper str [] builder = builder
-- helper str (x:y:z:code) builder = helper str (code) (builder ++ [(str !!((read $ x:y:z:"" :: Int) -1))])