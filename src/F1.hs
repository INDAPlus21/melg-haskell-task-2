module F1 where

-- Fibbonnaci
fibbottomup:: Integer -> Integer -> (Integer, Integer) -> Integer
fibbottomup stop depth vals = 
    if depth == stop then
        snd vals -- Reached stop
    else
        fibbottomup stop (depth + 1) (snd vals, fst vals + snd vals) -- Continue upwards

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fibbottomup n 2 (1, 1)

-- Röverspråk
vowels = ['a', 'e', 'o','u','i','y']

rovarsprakconsonant:: Char -> String
rovarsprakconsonant c = [c] ++ ['o'] ++ [c]

rovarsprakchar:: Char -> String
rovarsprakchar c = if c `elem` vowels then [c] else rovarsprakconsonant c

rovarsprak:: String -> String
rovarsprak s = rovarsprakloop 0 s []

rovarsprakloop:: Int -> String -> String -> String
rovarsprakloop pos org new = if pos == (length org) then new
    else rovarsprakloop (pos + 1) org (new ++ rovarsprakchar (org!!pos))

karpsravor:: String -> String
karpsravor s = karpsravorloop 0 s []

karpsravorloop:: Int -> String -> String -> String
karpsravorloop pos org new = if pos >= (length org) then new
    else if (org!!pos) `elem` vowels then 
        karpsravorloop (pos + 1) org (new ++ [org!!pos])
        else karpsravorloop (pos + 3) org (new ++ [org!!pos])

medellangd s = 1.0
skyffla s = s