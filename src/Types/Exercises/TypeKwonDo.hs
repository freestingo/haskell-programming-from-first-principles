module Types.Exercises.TypeKwonDo where

-- 1)

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h n = g $ f n

-- 2)

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w $ q a

-- 4)

munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xToY yToWZTuple x = fst $ yToWZTuple $ xToY x

dotlessMunge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
dotlessMunge xToY yToWZTuple = fst . yToWZTuple . xToY

