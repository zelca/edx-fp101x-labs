module Test where

fxs = 1 : [x + 1 | x <- fxs]

e10 (x,y) = [x,y]

double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

p [] = 1
p (x : xs) = x * p xs

qsort [] = []
qsort (x : xs) = qsort l ++ [x] ++ qsort s
  where s = [a | a <- xs, a < x || a == x]
        l = [b | b <- xs, b > x ]

swap (x,y) = (y,x)

pair x y = (x,y)

palindrome xs = reverse xs == xs

twice f x = f (f x)

safetail xs
  | null xs = []
  | otherwise = tail xs

safetail2 xs = tail xs
safetail2 [] = []

safetail3 [x] = [x]

safetail4 = \xs ->
  case xs of
    [] -> []
    (_ : xs) -> xs

putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >>= \ x -> putChar '\n'

sequence' ms = foldr (>>) (return ()) ms

foldl' f a [] = a
foldl' f a (x:xs) = foldl' f (f a x) xs

foldLeftM f a [] = return a
foldLeftM f a (x:xs) = foldLeftM f (f a (>>=) x) xs

dup a = (a, a)
t = dup . dup . dup

h g f = (f . g) $ f

fix = h fix

f = \f n -> if (n == 0) then 1 else n * f (n - 1)

k = fix $ f


