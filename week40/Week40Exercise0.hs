module Week40Exercise0 where

data Palindrome = Odd String String | Even String
    --deriving (Eq, Show)

createPalindrome :: String -> Palindrome
createPalindrome str = if odd $ length str then Odd (init half) ([last half]) else Even half where half = take ((1+length str) `div` 2) str

palindrome :: String -> Maybe Palindrome
palindrome str = if str == (reverse str) then Just (createPalindrome str) else Nothing

toString :: Palindrome -> String
toString (Odd x y) = x ++ y ++ (reverse x)
toString (Even x) = x ++ (reverse x)

main = print(toString (createPalindrome "aba"))