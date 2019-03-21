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

compress [] = []
compress [x] = [x]
compress xs = (compress (init xs)) ++ [y | y <- [last xs], not (elem y (init xs))]

compact [] = []
compact [x] = [x]
compact xs = (replicate (howManyElem (head xs) xs) (head xs)) ++ (compact (removeAllEFromList (head xs) xs))

removeAllEFromList e [] = []
removeAllEFromList e (x:xs) = if (e == x) then (removeAllEFromList e xs) else (x : (removeAllEFromList e xs))   
 
howManyElem e xs = length ([y | y <- xs, (e == y)])

encode [] = []
encode xs =  ((head xs), (howManyElem (head xs) xs)) : (encode (removeAllEFromList (head xs) xs))

split [] i = []
split xs i = [take i xs] ++ [drop i xs] 

slice [] imin imax = []
slice xs imin imax = drop (imin - 1) (take imax xs)

insertAt el pos xs = (take (pos - 1) xs) ++ [el] ++ (drop (pos - 1) xs)

minList [x] = x
minList (x:xs) = if (x < (minList xs)) then x else minList xs

removeOneEFromList e (x:xs) | e == x = xs
                | otherwise = x:(removeOneEFromList e xs)

selectionSort [] = []
selectionSort xs = x:ys 
    where
        x = minList xs
        ys = selectionSort (removeOneEFromList x xs)

mySum xs = foldr (+) 0 xs

maxList xs = foldr (max) 10 xs

buildPalindrome [] = []
buildPalindrome xs = xs ++ [(last xs)] ++ buildPalindromeWithoutInit (init xs) 
buildPalindromeWithoutInit [] = []
buildPalindromeWithoutInit xs = [last xs] ++ buildPalindromeWithoutInit (init xs)  

mean xs = mySum xs / meuLength xs 

myAppend xs ys = foldr (:) ys xs
 