module AlgebraicDatatypes.Exercises.HuttonsRazor where

import Data.List (intercalate)

{-|
   Hutton's Razor is a very simple expression language that expresses
   integer literals and addition of values in that expression language.
   The “trick” to it is that it’s recursive and the two expressions you’re
   summing together could be literals or themselves further addition operations.
   This sort of datatype is stereotypical of expression languages used to
   motivate ideas in research papers and functional pearls.
   Evaluating or folding a datatype is also in some sense what you're doing
   most of the time while programming anyway.
-}

data Expr = Lit Integer | Add Expr Expr

{-|
   Your first task is to write the `eval` function
   which reduces an expression to a final sum.
-}
eval :: Expr -> Integer
eval (Lit n) = n
eval (Add x y) = (eval x) + (eval y)

{-|
   Write a printer for the expressions.
-}
printExpr :: Expr -> String
printExpr = intercalate " + " . map show . exprList
   where exprList (Lit n) = [n]
         exprList (Add x y) = (exprList x) ++ (exprList y)

-- test data
a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2
