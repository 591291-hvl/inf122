module Week42Exercise0 where


applyFunctions :: [a -> b] -> [a] -> [b]
applyFunctions [] list = []
applyFunctions functions [] = []
applyFunctions (f:functions) (l:list) = (f l) : applyFunctions functions list

-- main = print(applyFunctions [(+3), (^2), (*5)] [4, 3, 7, 2, 9])
main = print(applyFunctions [(++ "someday"), (++ "today"), (++ "tomorrow"), (++ "yesterday")]  ["Change lightbulb: ", "Do the dishes: ", "Shower: ", "Mom's birthday: "])