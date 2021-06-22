module BasicDatatypes.Exercises where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs n = if n > 0 then n else (-n)

juggleTuples :: (a, b) -> (c, d) -> ((b, d), (a, c))
juggleTuples (a, b) (c, d) = ((b, d), (a, c))
