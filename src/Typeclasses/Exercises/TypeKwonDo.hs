module Typeclasses.Exercises.TypeKwonDo where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = aToB a == b

arith :: Num b => (a -> b)
               -> Integer
               -> a
               -> b
arith aToB n a = (aToB a) + (fromInteger n)

