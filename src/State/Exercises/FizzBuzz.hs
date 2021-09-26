module State.Exercises.FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = show n ++ " - FizzBuzz"
           | n `mod` 5 == 0  = show n ++ " - Buzz"
           | n `mod` 3 == 0  = show n ++ " - Fizz"
           | otherwise       = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to = fizzBuzzList [from..to]

doFizzBuzz :: IO ()
doFizzBuzz = mapM_ putStrLn $ reverse (fizzBuzzList [1..10] >> fizzBuzzList [1..10])
