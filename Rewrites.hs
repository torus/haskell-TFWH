module Rewrites (rewrites) where

import Expressions
import Laws (Equation)
import Matchings (match)
import Substitutions (apply)
import Utilities (anyOne, segments)

anyOne :: (a -> [a]) -> [a] -> [[a]]
anyOne f []       = []
anyOne f (x : xs) = [x' : xs | x' <- f x]
                 ++ [x : xs' | xs' <- anyOne f xs]

rewrites :: Equation -> Expr -> [Expr]
rewrites eqn (Compose as)
  = map Compose
        (rewritesSeg eqn as ++ anyOne (rewritesA eqn) as)

rewritesSeg :: Equation -> [Atom] -> [[Atom]]
rewritesSeg (e1, e2) as
  = [ as1 ++ deCompose (apply sub e2) ++ as3
    | (as1, as2, as3) <- segments as
    , sub <- match (e1, Compose as2) ]
