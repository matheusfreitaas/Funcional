xor a b = (a && not b) || (not a && b) 

impl a b = (not a || b)

equiv a b = (impl a b) && (impl b a)

square a = a * a

pow x y = product (replicate y x)

fatorial 0 = 1
fatorial 1 = 1
fatorial x = x * fatorial (x-1)  

isPrime 1 = True
isPrime 2 = True
isPrime 3 = True
isPrime x = isPrimeList x [2..(x-1)]

isPrimeList x [] = True
isPrimeList x (head:tail) = if (mod x head == 0) then False else isPrimeList x tail

fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

mdc x y = if (y == 0) then x else mdc y (mod x y)

coprimo x y = if (mdc x y == 1) then True else False


