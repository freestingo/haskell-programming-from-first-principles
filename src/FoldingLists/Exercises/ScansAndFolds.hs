module FoldingLists.Exercises.ScansAndFolds where

import Control.Applicative (liftA3)

-- infinite Fibonacci sequence generator
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

-- infinite factorial generator
facts :: [Integer]
facts = scanl (*) 1 [2..]

factorial :: Int -> [Integer]
factorial = flip take $ facts

------------------------------------------
   -- REWRITING OLD FUNCTIONS
------------------------------------------

myFoldOr :: [Bool] -> Bool
myFoldOr = foldr (||) False

myFoldAny :: (a -> Bool) -> [a] -> Bool
myFoldAny f = foldr (\x acc -> f x || acc) False

myFoldElem :: Eq a => a -> [a] -> Bool
myFoldElem a = foldr (\x acc -> a == x || acc) False

myFoldElem' :: Eq a => a -> [a] -> Bool
myFoldElem' a = myFoldAny (== a)

myFoldReverse :: [a] -> [a]
myFoldReverse = foldl (flip (:)) []

myFoldMap :: (a -> b) -> [a] -> [b]
myFoldMap f = foldr (\x acc -> f x : acc) []

myFoldFilter :: (a -> Bool) -> [a] -> [a]
myFoldFilter f = foldr doFilter []
  where doFilter x acc
          | f x = x : acc
          | otherwise = acc

myFoldSquish :: [[a]] -> [a]
myFoldSquish = foldr (++) []

myFoldSquishMap :: (a -> [b]) -> [a] -> [b]
myFoldSquishMap f = foldr (\x acc -> (f x) ++ acc) []

myFoldSquishAgain :: [[a]] -> [a]
myFoldSquishAgain = myFoldSquishMap id

myFoldMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myFoldMaximumBy f = foldr order Nothing
  where order x Nothing = Just x
        order x (Just acc) = if (f x acc == GT) then (Just x) else (Just acc)

myFoldMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myFoldMinimumBy f = myFoldMaximumBy $ flip f


------------------------------------------
   -- MISC
------------------------------------------

{-|
   Given the following sets of consonants and vowels,
   write a function that takes inputs from `stops` and `vowels`
   and makes 3-tuples of all possible stop-vowel-stop combinations.
-}
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combinations :: [(Char, Char, Char)]
combinations = [ (x, y, z) | x <- stops
                           , y <- vowels
                           , z <- stops
               ]

{-|
   Remember the vowels and stops exercise in the folds chapter?
   Write the function to generate the possible combinations of three
   input lists using `liftA3` from `Control.Applicative`.
-}
combinationsLift :: [a] -> [b] -> [c] -> [(a, b, c)]
combinationsLift = liftA3 (,,)

{-|
   Modify that function so that it only returns
   the combinations that begin with a `p`.
-}
pCombinations :: [(Char, Char, Char)]
pCombinations = [ (x, y, z) | x <- stops
                            , x == 'p'
                            , y <- vowels
                            , z <- stops
                ]

{-|
   Now set up lists of nouns and verbs (instead of stops
   and vowels) and modify the function to make tuples
   representing possible noun-verb-noun sentences.
-}
nouns :: [String]
nouns = ["pippo", "candle", "computer", "microphone"]

verbs :: [String]
verbs = ["eat", "poop", "drink", "say", "repeat"]

sentenceGenerator :: [(String, String, String)]
sentenceGenerator = [ (noun, verb, noun')
                    | noun  <- nouns
                    , verb  <- verbs
                    , noun' <- nouns
                ]

avgWordLength :: Fractional a => String -> a
avgWordLength s = (fromIntegral . sum . map length . words $ s) / (fromIntegral . length . words $ s)

