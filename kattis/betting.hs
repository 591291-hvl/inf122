main :: IO ()
main =
    getLine >>= \a -> let nmb = fromIntegral $ read a in
    putStrLn (show((100) / (nmb))) >>
    putStrLn (show((100) / (100-(nmb))))

-- main :: IO ()
-- main = do
--     a <- getLine
--     let nmb = fromIntegral $ read a
--     putStrLn $ show $ (100) / (nmb)
--     putStrLn $ show $ (100) / (100-nmb)
