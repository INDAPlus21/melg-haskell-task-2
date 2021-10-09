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

rovarsprak s = s
karpsravor s = s
medellangd s = 1.0
skyffla s = s