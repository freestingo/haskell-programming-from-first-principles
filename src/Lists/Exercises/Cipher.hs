module Lists.Exercises.Cipher where

import Data.Char

encrypt :: Int -> String -> String
encrypt steps = map chr . map (+ steps) . map ord
