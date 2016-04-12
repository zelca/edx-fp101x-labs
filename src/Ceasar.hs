module Ceasar where

import Data.Char

lower2int c = ord c - ord 'a'

upper2int c = ord c - ord 'A'

int2lower n = chr (ord 'a' + n)
int2upper n = chr (ord 'A' + n)

shift n c 
  | isLower c = int2lower ((lower2int c + n) `mod` 26)
  | isUpper c = int2upper ((upper2int c + n) `mod` 26)
  | otherwise = c

encode n xs = [shift n x | x <- xs]

