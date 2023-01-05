main :: IO ()
main = do
    line <- getLine
    print $ helper (map read (words line))

helper :: (Floating a) => [a] -> a
helper (a:b:c:d:_) = (sqrt $ (-a+b+c+d)*(a-b+c+d)*(a+b-c+d)*(a+b+c-d))/4