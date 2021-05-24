module Laws
  ( Law (Law), LawName, law, sortLaws
  , Equation, equation
  ) where

import Expressions
import Parsing
import Data.List (partition)

data Law      = Law LawName Equation
type LawName  = String
type Equation = (Expr, Expr)

law :: Parser Law
law = do { name <- upto ':'
         ; eqn  <- equation
         ; return (Law name eqn)
         }

upto :: Char -> Parser String
upto c = Parser (\ s ->
                   let (xs, ys) = break (== c) s in
                   if null ys then []
                   else [(xs, tail ys)])

equation :: Parser Equation
equation = do { e1 <- expr
              ; symbol "="
              ; e2 <- expr
              ; return (e1, e2)
              }

instance Show Law where
  showsPrec _ (Law name (e1, e2))
    = showString name
    . showString ": "
    . shows e1
    . showString " = "
    . shows e2

sortLaws :: [Law] -> [Law]
sortLaws = simple ++ others ++ defns
  where
    (simple, nonsimple) = partition isSimple laws
    (defns, others)     = partition isDefn nonsimple

partition p xs = (filter p xs, filter (not . p) xs)

isSimple (Law _ (Compose as1, Compose as2))
  = length as1 > length as2

isDefn (Law _ (Compose [Con f es], _))
         = all isVar es
isDefn _ = False

isVar (Compose [Var _]) = True
isVar _                 = False

simplify :: [String] -> String -> Calculation
simplify strings string
  = let laws = map (parse law) strings
        e    = parse expr string
    in calculate laws e

prove :: [String] -> String -> Calculation
prove strings string
  = let laws     = map (parse law) strings
        (e1, e2) = parse equation string
    in paste (calculate laws e1) (calculate laws e2)
