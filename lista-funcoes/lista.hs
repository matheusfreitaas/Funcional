divTuple (x, 0) = undefined
divTuple (1, 1) = 1
divTuple (x, 1) = x/1
divTuple (x, y) = x/y

somatorio a b = sum [a..b]

somatorioRecursivo a b = if (a + 1 == b) then a + b else a + somatorioRecursivo (a+1) b

square x = x*x

sumSquares x y = square x + square y

highOrderSum f a b = sum (map f [a,b])

hoSumSquares a b = highOrderSum square a b

isEven a = if mod a 2 == 0 then True else False

mapFilter f p xs = filter isEven (map f xs)



