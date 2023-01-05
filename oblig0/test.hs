-- Compute the average of a list of values
average :: (Fractional a) => [a] -> a
average a = (sum a) / fromIntegral(length a)


-- Count the number of zero-crossings in a signal represented by a list
zeroCrossings :: (Num a, Ord a) => [a] -> Integer
zeroCrossings (x:[]) = 0
zeroCrossings (x:y:list) 
    | x >= 0 && y <= 0 = 1 + zeroCrossings (y : list)
    | x <= 0 && y >= 0 = 1 + zeroCrossings (y : list)
    | otherwise = zeroCrossings (y : list)

-- Extend a finite signal with an infinite constant past
extend ::  Num a => [a] -> [a]
extend (x:[]) = x : extend [x]
extend (x:xs) = x : extend xs


-- Apply a filter to a list of values
applyFilter :: (Num a, Floating a) => ([a] -> a) -> [a] -> [a]
applyFilter fil = map fil . iterate tail . extend

-- A simple lowpass filter with adjustable cut-off
lpf :: (Fractional a) => Integer -> [a] -> a
lpf numb list = average (take (fromIntegral numb) list)

-- A simple high pass filter with adjustable cut-off
hpf :: (Floating a) => Integer -> [a] -> a
hpf numb list = (head list) - lpf numb list 

lowPassCutoff :: Integer
lowPassCutoff = 30
highPassCutoff :: Integer
highPassCutoff = 10

sumData :: [(Double, Double, Double)] -> [Double]
sumData [] = []
sumData list = map (\(a, b, c) -> a + b + c) list

lengthData :: [Double] -> Int
lengthData [] = 0
lengthData list =  length list

processData :: [Double] -> [Double]
processData summed = applyFilter (hpf highPassCutoff) $ applyFilter (lpf lowPassCutoff) $ reverse summed


main = print(processData [1,5,10,4])