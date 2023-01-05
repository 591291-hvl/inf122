main :: IO()
main =
    getLine >>= \line1 ->
    getLine >>= \line2 ->
    print $ round $ (\(x:y:z) -> (x-y)*(9/10) - (sum (map read (words line2)))) (map read (words line1))


-- main :: IO()
-- main = do
--     line1 <- getLine
--     line2 <- getLine
--     print $ round $ (\(x:y:z) -> (x-y)*(9/10) - (sum (map read (words line2)))) (map read (words line1))