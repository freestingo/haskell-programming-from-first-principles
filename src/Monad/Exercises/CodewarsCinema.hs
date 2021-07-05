module Monad.Exercises.CodewarsCinema where

import Data.List
import Control.Monad
import Control.Monad.Trans.Maybe
import System.Exit
import qualified System.Random.Shuffle as S

{-|
   Given a number of boys and girls, find a seating disposition such that
   every girl sits at least next to one boy, and viceversa.
   If no such disposition is possible, return `Nothing`.
-}
seatings :: Int -> Int -> Maybe String
seatings boys girls
   | pairs < 0 = Nothing
   | otherwise = Just $ concat $ replicate triples (if boys > girls then "BGB" else "GBG") ++ replicate pairs "GB"
     where triples = abs $ boys - girls
           pairs = min boys girls - triples


seatings' :: Int -> Int -> IO String
seatings' boys girls = do
   guard $ requirements boys girls
   shuffled <- S.shuffleM $ replicate boys 'B' ++ replicate girls 'G'
   if isGoodSeating shuffled
      then do
         putStrLn $ "found it! " ++ shuffled
         return shuffled
      else do
         putStrLn $ shuffled ++ " | trying again..."
         seatings' boys girls
   where requirements b g = (b > div g 2) && (g > div b 2)
         isGoodSeating str = not $ "BBB" `isInfixOf` str
                                || "GGG" `isInfixOf` str
                                || ((=="BB") . take 2 $ str)
                                || ((=="GG") . take 2 $ str)
                                || ((=="BB") . take 2 . reverse $ str)
                                || ((=="GG") . take 2 . reverse $ str)

