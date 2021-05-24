module Utilities where

parts :: Int -> [a] -> [[[a]]]
parts 0 [] = [[]]
parts 0 as = []
parts n as = [ bs : bss
             | (bs, cs) <- splits as
             , bss <- parts (n - 1) cs ]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [ x : ys | x <- xs, ys <- yss ]
  where
    yss = cp xss

splits :: [a] -> [([a], [a])]
splits []       = [([], [])]
splits (a : as) = [([], a : as)]
               ++ [(a : as1, as2) | (as1, as2) <- splits as]

segments as = [ (as1, as2, as3)
              | (as1, bs)  <- splits as
              , (as2, as3) <- splits bs
              ]

compose :: [a -> a] -> a -> a
compose = foldr (.) id
