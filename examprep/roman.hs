-- Number to roman
numberToRoman :: Int -> String
numberToRoman nmb = numberToRomanConverter (numberToRomanHelper nmb "") ""

numberToRomanHelper :: Int -> String -> String
numberToRomanHelper 0 acc = acc
numberToRomanHelper nmb acc 
    | nmb >= 1000 = numberToRomanHelper (nmb - 1000) (acc++"M")
    | nmb >= 500 = numberToRomanHelper (nmb - 500) (acc++"D")
    | nmb >= 100 = numberToRomanHelper (nmb - 100) (acc++"C")
    | nmb >= 50 = numberToRomanHelper (nmb - 50) (acc++"L")
    | nmb >= 10 = numberToRomanHelper (nmb - 10) (acc++"X")
    | nmb >= 5 = numberToRomanHelper (nmb - 5) (acc++"V")
    | otherwise = numberToRomanHelper (nmb - 1) (acc++"I")

numberToRomanConverter :: String -> String -> String
numberToRomanConverter "" acc = acc 
numberToRomanConverter str acc 
    | length str >= 4 && compareFour str = numberToRomanConverter (drop 4 str) (acc ++ (converter $ take 4 str))
    | otherwise = numberToRomanConverter (drop 1 str) (acc ++ [head str])

compareFour :: String -> Bool
compareFour str = (\(a:b:c:d:_) -> a==b && b==c && c==d) str

converter :: String -> String
converter str 
    | head str == 'D' = "CD"
    | head str == 'X' = "XL" 
    | head str == 'I' = "IV"
    | otherwise = str

-- main = putStrLn $ numberToRoman 1234



romanToNumber :: String -> Int
romanToNumber str = romanToNumberHelper (romanFixer str "") 0

romanToNumberHelper :: String -> Int -> Int 
romanToNumberHelper [] acc = acc
romanToNumberHelper str acc
    | head str == 'M' = romanToNumberHelper (tail str) (acc+1000)
    | head str == 'D' = romanToNumberHelper (tail str) (acc+500)
    | head str == 'C' = romanToNumberHelper (tail str) (acc+100)
    | head str == 'L' = romanToNumberHelper (tail str) (acc+50)
    | head str == 'X' = romanToNumberHelper (tail str) (acc+10)
    | head str == 'V' = romanToNumberHelper (tail str) (acc+5)
    | otherwise = romanToNumberHelper (tail str) (acc+1)

romanFixer :: String -> String -> String
romanFixer [] acc = acc
romanFixer (_:[]) acc = acc
romanFixer (x:y:str) acc 
    | x == 'C' && y == 'D' = romanFixer (str) (acc ++ "DDDD")
    | x == 'X' && y == 'L' = romanFixer (str) (acc ++ "XXXX")
    | x == 'I' && y == 'V' = romanFixer (str) (acc ++ "IIII")
    | otherwise = romanFixer (y:str) (acc ++ [x])

main = print $ romanToNumber "MCCXXXIV" 