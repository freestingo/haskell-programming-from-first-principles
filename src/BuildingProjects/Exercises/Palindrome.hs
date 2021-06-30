module BuildingProjects.Exercises.Palindrome where

import Data.Char
import Control.Monad
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome =
    let cleanLine = filter (flip elem alphabet) . map toLower . filter (/=' ')
        alphabet = ['a'..'z']
    in forever $ do
        l <- getLine
        case (cleanLine l == reverse (cleanLine l)) of
          True -> putStrLn "It's a palindrome!"
          False -> do
              putStrLn "It's not a palindrome :("
              exitSuccess
