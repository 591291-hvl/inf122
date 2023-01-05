module Week37Exercise1 where
import Data.List(nubBy)


comparetuple :: (Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer, Integer) -> Bool
comparetuple (a1, b1, c1, d1) (a2, b2, c2, d2) = 
    (a1 == a2 || a1 == b2 || a1 == c2 || a1 == d2) &&
    (b1 == a2 || b1 == b2 || b1 == c2 || b1 == d2) &&
    (c1 == a2 || c1 == b2 || c1 == c2 || c1 == d2) &&
    (d1 == a2 || d1 == b2 || d1 == c2 || d1 == d2)


--equalCubeSum :: Integer -> [(Integer, Integer, Integer, Integer)]
--equalCubeSum n = nubBy comparetuple $ filter (/=(0,0,0,0)) [if x /= y && fst x /= snd y then (fst x, snd x, fst y, snd y) else (0,0,0,0)  | x <- filter ((>0).snd) [if 1 + n^3 == (x^3 + y^3) then ( x, y)  else ( 0, 0) | x <- [1..n], y <- [1..n]], y <- filter ((>0).snd) [if 1 + n^3 == (x^3 + y^3) then ( x, y)  else ( 0, 0) | x <- [1..n], y <- [1..n]]]
equalCubeSum :: Integer -> [(Integer, Integer, Integer, Integer)]
equalCubeSum n = nubBy comparetuple $ filter (/=(0,0,0,0)) [if a*a*a + b*b*b == c*c*c + d*d*d && (a,b) /= (c,d) && (a,b) /= (d,c) then (a,b,c,d) else (0,0,0,0) | a <- [1..n], b <- [1..n], c <- [1..n], d <- [1..n]]

--main = print(equalCubeSum 30)
