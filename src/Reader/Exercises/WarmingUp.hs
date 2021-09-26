module Reader.Exercises.WarmingUp where

import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

monadic :: [Char] -> ([Char], [Char])
monadic = do
    c <- cap
    r <- rev
    return (c, r)
































-- cap :: [Char] -> [Char]
-- cap = map toUpper

-- rev :: [Char] -> [Char]
-- rev = reverse

-- composed :: [Char] -> [Char]
-- composed = cap . rev

-- fmapped :: [Char] -> [Char]
-- fmapped = fmap cap rev

-- tupled :: [Char] -> ([Char], [Char])
-- tupled = (,) <$> cap <*> rev

-- monaded :: [Char] -> ([Char], [Char])
-- monaded = do
--     capitalized <- cap
--     reversed <- rev
--     return (capitalized, reversed)

-- binded :: [Char] -> ([Char], [Char])
-- binded = cap >>= \c -> rev >>= \r -> return (c, r)

