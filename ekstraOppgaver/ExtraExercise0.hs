module ExtraExercise0 where

doubleNames :: [String] -> [String]
doubleNames x = [ a ++ "-" ++ b| a <- x, b <- x , a /= b]

main = print $ doubleNames ["Marie", "Hilde", "Marit", "Elin"]