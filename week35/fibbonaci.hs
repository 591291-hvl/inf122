fib :: (Integral a) => a -> [a]
fib 1 = [1]
fib 2 = [1,1]
fib n = (fib (n-1)) ++ [last (fib (n-1)) + last (fib (n-2))] 

main = print(fib 5)
--print([1,1] ++ [last [1,1] + last (init [1,1])])  