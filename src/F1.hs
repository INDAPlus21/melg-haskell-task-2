module F1 where
import Data.Char (isAlpha )

-- Fibbonnaci
fibbottomup:: Integer -> Integer -> (Integer, Integer) -> Integer
fibbottomup stop depth vals = 
    if depth == stop then
        snd vals
    else
        fibbottomup stop (depth + 1) (snd vals, fst vals + snd vals)

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
        karpsravorloop (pos + 1) org (new ++ [org!!pos]) -- Jump to next letter
        else karpsravorloop (pos + 3) org (new ++ [org!!pos]) -- Jump three letters forwards

-- Medellängd
medellangd:: String -> Float
medellangd s = (fst (medellangdloop 0 s 0 1 0)) / (snd (medellangdloop 0 s 0 1 0))

medellangdloop:: Int -> String -> Float -> Float -> Float -> (Float, Float)
medellangdloop pos s chars words currentlen = if pos >= (length s) then 
    (chars + currentlen, if currentlen == 0 then words -1 else words) else -- Remove empty word at end
    if isAlpha(s!!pos) then medellangdloop (pos + 1) s chars words (currentlen + 1) else -- Add to current length if letter
    medellangdloop (pos + 1) s (chars + currentlen) (if currentlen == 0 then words else words + 1) 0 -- Add word if not letter

-- Skyffla
skyffla:: [a] -> [a]
skyffla s = fst (skyfflarec [] s)

skyfflarec:: [a] -> [a] -> ([a], [a])
skyfflarec finished left =
    if (length left) == 0 then
        (finished, left)
    else
        skyfflarec (finished ++ [x | (x, i) <- zip left [0..], i `mod` 2 == 0]) -- Add elements with even indexes
        ([x | (x, i) <- zip left [0..], i `mod` 2 == 1]) -- Continue with elements with odd indexes