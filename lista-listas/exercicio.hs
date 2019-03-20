meuLast [] = error "Lista Vazia!"
meuLast xs = last xs

penultimo [] = error "Lista sem penultimo"
penultimo [x] = error "Lista sem penultimo"
penultimo xs = if (length xs) == 2 then head xs else penultimo (tail xs) 

elementAt i xs = head (drop (i - 1) xs)

meuLength [] = 0
meuLength [x] = 1
meuLength xs = 1 + (meuLength (tail xs))

meuReverso [] = []
meuReverso [x] = [x]
meuReverso xs = (last xs) : (meuReverso (init xs))

isPalindrome xs = (xs == (reverse xs))

 

