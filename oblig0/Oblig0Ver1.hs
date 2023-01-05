module Main where

import Oblig0Common
  ( applyFilter,
    highPassCutoff,
    hpf,
    lowPassCutoff,
    lpf,
  )
import System.IO

processData :: [Double] -> [Double]
processData summed = applyFilter (hpf highPassCutoff) $ applyFilter (lpf lowPassCutoff) $ summed

printHelp :: Bool -> IO()
printHelp True = do 
    putStrLn ("Step!")
    hFlush stdout
printHelp False = return()

zeroCrossings :: (Num a, Ord a) => [a] -> Integer
zeroCrossings [] = 0
zeroCrossings (x:[]) = 0
zeroCrossings (x:y:list) 
    | x >= 0 && y <= 0 = 1 
    | x <= 0 && y >= 0 = 1
    | otherwise = 0

    
getLineFromFile :: IO(Double)
getLineFromFile = do
    line <- getLine
    let number = (\(a, b, c) -> a + b + c) (read line)
    return (number)

rekPrint :: [Double] -> Integer -> Bool-> IO() 
rekPrint [] crossings up = do --Start condition
    line <- getLineFromFile
    let processedData = processData [line]
    rekPrint [line] crossings up
rekPrint x crossings up = do 
    endOfLine <- isEOF --End condition
    if endOfLine
        then print (crossings)
        else do --Everything in between condition:)

        line <- getLineFromFile
        let processedData = take 2 (processData (line:x))
        let ifCrossed = zeroCrossings (processedData)
    
        printHelp (ifCrossed == 1 && up)
    
        if not ((ifCrossed == 1)  == up) then rekPrint (line:x) (crossings + ifCrossed) (True) else rekPrint (line:x) (crossings) (False)

main = rekPrint [] 0 False