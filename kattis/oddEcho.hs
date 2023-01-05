main :: IO ()
main =
    getLine >>= \a ->
    helper (read a) True

helper ::Int -> Bool -> IO ()
helper 0 _ = return () 
helper n True =
    getLine >>= \a ->
    putStrLn a >>
    helper (n-1) False
helper n False =
    getLine >>= \a ->
    helper (n-1) True
