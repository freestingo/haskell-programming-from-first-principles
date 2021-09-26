module Reader.Practice.FunctionFunctor where

import Control.Applicative

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

{-|
   Feeding a single argument to the (*2) and (+10),
   while the two results form the two arguments to (+).
-}
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

{-|
   This will do precisely the same thing as the `Applicative` example,
   but this time the context is monadic.
-}
boopDoop :: Integer -> Integer
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)
