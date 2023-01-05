module Week38Exercise1 where

mySplitAt :: Integer  -> [a] -> ([a],[a])
mySplitAt n a = mySplitAtHelp (n) a ([],[])

--mySplitAtHelp :: Integer  -> [a] -> ([a],[a]) -> ([a],[a])
--mySplitAtHelp n [] (l1,l2) = ([],[])
--mySplitAtHelp n a (l1,l2) 
--    | n < 0 = ([],a)
--    | n == 0 = (l1, a)
--    | n > (toInteger $ length a) = (a, l2)
--    | otherwise = mySplitAtHelp (n-1) (tail a) (l1 ++ [head a], l2)

mySplitAtHelp :: Integer  -> [a] -> ([a],[a]) -> ([a],[a])
mySplitAtHelp n [] (l1,l2) = (l1,l2)
mySplitAtHelp 0 a (l1,l2) = (l1,a)
mySplitAtHelp n a (l1,l2) 
    | n <= 0 = (l1, a)
    | otherwise = mySplitAtHelp (n-1) (tail a) (l1 ++ [head a], l2)

main = print(mySplitAt (-2) [1,2,3,4,5])

