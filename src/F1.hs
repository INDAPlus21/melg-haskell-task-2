module F1 where
import Data.Char (isAlpha)

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

rovarsprakchar:: Char -> String
rovarsprakchar c = if c `elem` vowels then [c] else [c, 'o', c]

rovarsprak:: String -> String
rovarsprak s = reverse(rovarsprakloop s [])

rovarsprakloop:: String -> String -> String
rovarsprakloop org new = if org == [] then new
    else if (head org) `elem` vowels then
        rovarsprakloop (tail org) ((head org):new)
        else rovarsprakloop (tail org) ((head org):'o':(head org):new)

karpsravor:: String -> String
karpsravor s = reverse(karpsravorloop s [])

karpsravorloop:: String -> String -> String
karpsravorloop org new = if org == [] then new
    else if (head org) `elem` vowels then 
        karpsravorloop (tail org) ((head org):new) -- Jump to next letter
        else karpsravorloop (drop 3 org) ((head org):new) -- Jump three letters forwards

-- Medellängd
medellangd:: String -> Float
medellangd s = (fst (medellangdloop s 0 1 0)) / (snd (medellangdloop s 0 1 0))

medellangdloop:: String -> Float -> Float -> Float -> (Float, Float)
medellangdloop s chars words currentlen = if s == "" then 
    (chars + currentlen, if currentlen == 0 then words -1 else words) else -- Remove empty word at end
    if isAlpha(head s) then medellangdloop (tail s) chars words (currentlen + 1) else -- Add to current length if letter
    medellangdloop (tail s) (chars + currentlen) (if currentlen == 0 then words else words + 1) 0 -- Add word if not letter

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