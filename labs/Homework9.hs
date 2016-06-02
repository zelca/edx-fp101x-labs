module Homework9 where

-- this homework works only with Hugs
-- because of specific import and pattern matching

import Data.List
import Data.Char
import Hugs.IOExts (unsafeCoerce)

data Nat = Zero
         | Succ Nat
         deriving Show

-- exercise 0

test0 = Zero
test1 = Succ (Succ (Succ Zero))

nat2Integer1 Zero = 0
nat2Integer1 (Succ n) = nat2Integer1 n + 1

nat2Integer2 (Succ n) = nat2Integer2 n + 1
nat2Integer2 Zero = 0

nat2Integer3 (Succ n) = 1 + nat2Integer3 n
nat2Integer3 Zero = 0

nat2Integer4 = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1 : m n)]]

nat2Integer5 :: Nat -> Integer
nat2Integer5 = \ n -> genericLength [c | c <- show n, c == 'S']

nat2Integer6 :: Nat -> Int
nat2Integer6 = \ n -> length [c | c <- show n, c == 'S']

-- exercise 1

integer2Nat1 0 = Zero
integer2Nat1 (n+1) = Succ (integer2Nat1 n)

integer2Nat2 (n+1) = Succ (integer2Nat2 n)
integer2Nat2 0 = Zero

integer2Nat3 n = product [(unsafeCoerce c) :: Integer | c <- show n]

integer2Nat4 (n+1) = let m = integer2Nat2 n in Succ m
integer2Nat4 0 = Zero

-- exercise 2

add n (Succ m) = Succ (add m n)
add n Zero = n

-- exercise 4

data Tree = Leaf Integer
          | Node Tree Integer Tree

testTree = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 8))

occurs m (Leaf n) = m == n
occurs m (Node l n r)
  | m == n = True
  | m < n = occurs m l
  | otherwise = occurs m r

