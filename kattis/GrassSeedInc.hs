main :: IO()
main = do
    mul <- getLine
    n <- getLine
    helper (read n) (read mul) 0.0

helper :: Int -> Float -> Float -> IO () 
helper 0 mul acc = print $ (mul * acc)
helper n mul acc = do
    line <- getLine
    let kvd =  (\(x:y:_) ->x*y) $ map read (words line)
    helper (n-1) mul (acc + kvd)