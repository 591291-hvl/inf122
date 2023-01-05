module Week36Exercise2 where

halfPalindrome :: String -> Maybe String
halfPalindrome word = if word == reverse word then Just $ take ((length word `div`) 2) word else Nothing


decomposePalindrome :: String -> Maybe(String, Maybe Char)
decomposePalindrome word = case (halfPalindrome word) of 
    Just a -> Just (a, if even $ length word then Nothing else Just $ word !! ((length word `div`)2))
    a -> Nothing


createPalindrome :: String -> Maybe Char -> String
createPalindrome word a = case(a) of
    Just a -> word ++ [a] ++ reverse word
    a -> word ++ reverse word
