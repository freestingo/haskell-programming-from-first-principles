module AlgebraicDatatypes.Exercises.Cipher where

import Data.Char

{-|
   Vigenère cipher, or polyalphabetic substitution based on an arbitrary, fixed keyword.
   Example with keyword `ALLY`:
      MEET AT DAWN
   -> ALLY AL LYAL (A = 0 shift, L = 11 shift, etc...)
   -> MPPR AE OYWY
-}

shift :: (Int -> Int -> Int) -> Char -> Char -> Char
shift op offset ch = numToChar $ (charToNum ch) `op` (charToNum offset)
  where
    charToNum ch = ord ch - ord 'A'
    numToChar n = chr $ (n `mod` 26) + ord 'A'

vigenere :: String -> String -> String
vigenere secretKey = zipWith (shift (+)) (cycle secretKey) . concat . words

