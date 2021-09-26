module State.Practice.RandomNumbers where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- TODO find a better way than to use `error`
    _ -> error $ "unhandled value " ++ show n

rollDie :: State StdGen Die
rollDie = intToDie <$> state (randomR (1, 6))

rollThreeTimes' :: State StdGen (Die, Die, Die)
rollThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

rollNTimes :: Int -> [Die]
rollNTimes n = evalState (roll n) (mkStdGen 124)

roll :: Int -> State StdGen [Die]
roll n = replicateM n rollDie

{-|
   Implementation without `State`
-}
rollThreeTimes :: (Die, Die, Die)
rollThreeTimes = do
    let s = mkStdGen 124 -- 124 is the starting seed value
        (d1, s1) = randomR (1, 6) s
        (d2, s2) = randomR (1, 6) s1
        (d3, s3) = randomR (1, 6) s2
    (intToDie d1, intToDie d2, intToDie d3)

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
    where go :: Int -> Int -> StdGen -> Int
          go sum count gen
            | sum >= n = count
            | otherwise = let (die, nextGen) = randomR (1, 6) gen
                          in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go (0, []) 0
    where go :: (Int, [Die]) -> Int -> StdGen -> (Int, [Die])
          go log count gen
            | fst log >= n = log
            | otherwise = let (die, nextGen) = randomR (1, 6) gen
                          in go (fst log + die, snd log ++ [intToDie die]) (count + 1) nextGen

