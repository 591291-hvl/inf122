import Data.List

main :: IO()
main = do
    n <- getLine
    helper (read n)

helper :: Int -> IO()
helper 0 = return ()
helper n = do
    m <- getLine
    list <- helperHelper (read m) []
    print $ length $ nub list
    helper (n-1)

helperHelper :: Int -> [String] -> IO([String])
helperHelper 0 acc = return acc
helperHelper n acc = do
    line <- getLine
    helperHelper (n-1) (line : acc)
