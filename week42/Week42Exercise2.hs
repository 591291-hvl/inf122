module Week42Exercise2 where


reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- main = print(unwords $ map (reverse)  (words "sup dude"))
-- main = print $ reverseWord "Alice was beginning to get very tired of sitting by her sister on the bank and of having nothing to do"



sumIsEven2 :: Integer -> Integer -> Bool
sumIsEven2 = (\x y -> even (sum [x,y]))



sumIsEven :: Integer -> Integer -> Bool
sumIsEven = (even .) . (+)

main = print $ sumIsEven 1 2

-- f x y = x + y
-- g x = even x
-- h = (.) g . f

-- main = print $ h 1 2
