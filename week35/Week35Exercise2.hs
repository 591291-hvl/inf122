module Week35Exercise2 where

repeat2 :: [a]->[a]
repeat2 n = take (2 * length n )(cycle n)
